package fixbugs.mc

import org.scalacheck._
import java.util.Random

import org.scalatest._
import matchers._
import org.scalacheck.Prop.forAll
import prop.Checkers

import fixbugs.mc.ir._
import fixbugs.mc.Refiner.refine

class RefinerSpec extends Spec with Checkers with ShouldMatchers {

  val params = Test.Params(100,5,1,17,new Random,1,1)
    
  describe("Refinement") {
    it ("should have correct testing code") {
      assert(! refinedSpec(All(Future(True())))) 
      refine(All(Next(True()))) should equal (Not(Exists(Next(Not(True())))))
      refine(Exists(Future(NodePred("X")))) should equal (Exists(Until(True(),NodePred("X"))))
    }
    it ("should have correct refinement") {
      check(forAll((phi: NodeCondition) => refinedSpec(refine(phi))),params)
    }
  }
  
  def refinedSpec(phi:NodeCondition) : Boolean = {
    phi match {
    	case False() => true
        case True() => true
        case NodePred(key) => true
        case Not(phi) => refinedSpec(phi)
        case Or(l,r) => refinedSpec(l) && refinedSpec(r)
        case And(l,r) => refinedSpec(l) && refinedSpec(r)
        case Exists(Next(phi)) => refinedSpec(phi)
        case Exists(Global(phi)) => refinedSpec(phi)
        case Exists(Until(phi,psi)) => refinedSpec(phi)
        
        case x => false
    }
  }
  
  
}
