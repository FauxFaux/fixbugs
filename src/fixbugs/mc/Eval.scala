package fixbugs.mc;

import fixbugs.core.ir._;
import fixbugs.mc.sets._;
import scala.collection.mutable.{Map => MMap}

case class Node(lineNumber:Int){}

trait Evaluator {
  def eval(nc:NodeCondition):ClosedEnvironment[Int]
}


class Eval(nodes:Set[Int],domain:ClosedDomain[Int],succ:MMap[Int,Set[Int]],pred:MMap[Int,Set[Int]]) extends Evaluator {

	val imPred = Map() ++ pred
	val imSucc = Map() ++ succ
	
	// entry points are nodes with no predecessors
	val entry = nodes -- imPred.keys
	
	// exit points are nodes with no successors
	val exit = nodes -- imSucc.keys
  
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
            
            case Exists(Until(phi,psi)) => loop(eval(psi),eval(phi))
            
            case Exists(Global(phi)) =>  {
              // there must be a path where phi holds true at every step
              val phis = eval(phi)
              // initial: phis that are exit points 
              //println("GLOBAL"+phi)
              loop(phis.restrictKeyTo("current",exit),phis)
            }
            
            case x => throw new Exception("Unknown temporal IR element: "+x)            
        }
    }
    
    /**
     * Loop to compute fixed point 
     */
    def loop(init:ClosedEnvironment[Int],iter:ClosedEnvironment[Int]) = {
      // initially init 
      var values = init
      var prev = values
      //println(iter.allValues.map(x => x("current")))
      //println(init.allValues.map(x => x("current")))
      do {
        prev = values.copy
        //println(values.allValues.map(x => x("current")))
        
        // iteratively add predecessors that are in iter
        values = values union (values.mapKey("current",imPred) intersect iter)
      } while(prev != values)
      //println("Loop returning: "+values.allValues.map(x => x("current")))
      values
    }

}
