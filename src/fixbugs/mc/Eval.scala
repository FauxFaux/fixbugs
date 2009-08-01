package fixbugs.mc;

import fixbugs.mc.ir._;
import fixbugs.mc.sets._;
import scala.collection.mutable.{Map => MMap}

case class Node(lineNumber:Int){}

trait Evaluator {
  def eval(nc:NodeCondition):ClosedEnvironment[Int]
}

class Eval(domain:ClosedDomain[Int],succ:MMap[Int,Set[Int]],pred:MMap[Int,Set[Int]]) extends Evaluator {

	val imPred = Map() ++ pred
	val imSucc = Map() ++ succ
  
    def eval(nc:NodeCondition):ClosedEnvironment[Int] = {
        nc match {
            case False() => domain.none()
            case True() => domain.all()
            case NodePred(key) => domain.all().equalByKey(key,"current")
            case Not(phi) => eval(phi).negate()
            case Or(l,r) => eval(l) union eval(r)
            case And(l,r) => eval(l) intersect eval(r)

            // Temporal cases only EX,EG,EU due to refinement
            
            // move current to predecessor
            case Exists(Next(phi)) => eval(phi).mapKey("current",imPred)
            
            // TODO: global and until
            case Exists(Global(phi)) => {
              // there must be a path where phi holds true at every step
              val phis = eval(phi)
              // initial: x. x in phis and x has no successor
              val values = domain.none()
              do {
                // iteratively add predecessors where they are all in phis
              } while(true)
              values
            }
            
            case Exists(Until(phi,psi)) => {
              val phis = eval(phi)
              // initially - psis
              val values = eval(psi)
              // iteratively add predecessors in phis
              values
            }
            
            case x => throw new Exception("Unknown temporal IR element: "+x)
        }
    }

}
