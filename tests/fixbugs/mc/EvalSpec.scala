package fixbugs.mc

import org.scalatest._
import junit._
import matchers._

import fixbugs.mc._
import sets._
import fixbugs.core.ir._
import fixbugs.mc.Refiner.refine
import fixbugs.util.MapUtil._
import scala.collection.mutable.{HashMap => MMap}

class EvalSpec extends Spec with ShouldMatchers {

  def createFixtures = {
    val nodes = Set() ++ (0 to 4)
    val succ = (new MMap[Int,Set[Int]]()
    	+ (0->Set(1,2))
    	+ (2->Set(3,4))
    	+ (3->Set(0)))
    (nodes,
    new SetClosedDomain[Int](crossWith(Set(Map("X"->0,"Y"->2)),"_current",nodes)),
    succ,
    reverse(succ)
  )}
  
  def current[V](x:Iterable[Map[String,V]]) = x.map(v => v("_current"))
  
  describe("Evaluation") {
    val(nodes,dom,succ,pred) = createFixtures
	val env:Eval = new Eval(nodes,dom,succ,pred)
 
    //println(dom.all.allValues)
    
    it ("should have correct test data") {
      pred should equal (MMap(2 -> Set(0), 4 -> Set(2), 1 -> Set(0), 3 -> Set(2), 0 -> Set(3)))
    }
 
    it ("should have correct propostional logic") {
      val (tt,ff) = (dom.all,dom.none)
      env eval(True()) should equal (tt)
      env eval(False()) should equal (ff)
      env eval(Or(False(),True())) should equal (tt)
      env eval(Or(False(),False())) should equal (ff)
      env eval(And(False(),True())) should equal (ff)
      env eval(And(True(),True())) should equal (tt)
      env eval(Not(True())) should equal (ff)
      env eval(And(True(),Not(True()))) should equal (ff)
    }
    
    it("Temporal Predicates") {
      val x = (env eval NodePred("X") allValues)
      x should have size (1)
      x.toList(0) should (contain key ("_current") and contain value (0))
    }
    
	it ("Next Operator") {
	  val x = (env eval Exists(Next(NodePred("X"))) allValues)
      x should have size (1)
      x.toList(0) should (contain key ("_current") and contain value (3))
	}
	
	it ("Until and Global Operators") {
	  current(env eval refine(Exists(Future(NodePred("X")))) allValues) should equal (Set(0,2,3))
	  current(env eval refine(All(Future(NodePred("X")))) allValues) should equal (Set(0,3))
	}
  }
  
}
