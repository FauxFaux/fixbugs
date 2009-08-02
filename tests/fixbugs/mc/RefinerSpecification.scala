package fixbugs.mc

import org.scalacheck._
import java.util.Random

import org.scalacheck.Prop.forAll

import fixbugs.mc.ir._
import fixbugs.mc.Refiner.refine

object RefinerSpecification extends Properties("Refiner") {

  // TODO: figure out how to just write normal BDD type tests
  /*specify("refinement falses", (phi: NodeCondition) => {
    ! refinedSpec(All(Future(True())))
  })*/
  
  specify("refinement", (phi: NodeCondition) => refinedSpec(refine(phi)))
  
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
