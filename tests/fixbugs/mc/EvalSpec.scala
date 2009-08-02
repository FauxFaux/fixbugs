package fixbugs.mc

import org.scalatest._
import junit._
import matchers._

import fixbugs.mc._
import sets._
import ir._
import fixbugs.util.MapUtil.reverse
import scala.collection.mutable.{HashMap => MMap}

class EvalSpec extends Spec with ShouldMatchers {

  def createFixtures = {
    val succ = (new MMap[Int,Set[Int]]()
    	+ (0->Set(1,2))
    	+ (2->Set(3,4))
    	+ (3->Set(0)))
    (new SetClosedDomain[Int](Set(Map("X"->0,"Y"->2))),
    succ,
    reverse(succ)
  )}
  
  describe("Evaluation") {
    val(dom,succ,pred) = createFixtures
	val env:Eval = new Eval(dom,succ,pred)
    
    it("should have correct test data") {
      pred should equal (MMap(2 -> Set(0), 4 -> Set(2), 1 -> Set(0), 3 -> Set(2), 0 -> Set(3)))
    }
 
    it("should have correct basic logic") {
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
  }
}

