package fixbugs.mc

import fixbugs.core.ir._
import fixbugs.mc.sets._
import scala.collection.mutable.{Map => MMap}
import org.slf4j.{Logger,LoggerFactory}
import org.objectweb.asm.tree.MethodNode

case class Node(lineNumber:Int){}

trait Evaluator {
  def eval(mc:MethodNode,nc:SideCondition):ClosedEnvironment[Any]
}

class Eval(typeEnv:TypeEnvironment,iNodes:Set[Int],domain:ClosedDomain[Any],succ:MMap[Int,Set[Int]],pred:MMap[Int,Set[Int]]) extends Evaluator {

    val log = LoggerFactory getLogger(this getClass)

	val imPred = (Map() ++ pred).asInstanceOf[Map[Any,Set[Any]]]
	val imSucc = (Map() ++ succ).asInstanceOf[Map[Any,Set[Any]]]
    val nodes = iNodes.asInstanceOf[Set[Any]]
    val types = new TypeComparator(typeEnv)
	
	// entry points are nodes with no predecessors
	val entry = nodes -- imPred.keys
	
	// exit points are nodes with no successors
	val exit = nodes -- imSucc.keys

    // converts boolean predicate to domains
    def pred(p:Boolean) = if (p) domain.all() else domain.none()

    def eval(mc:MethodNode,sc:SideCondition):ClosedEnvironment[Any] = sc match {
        case SFalse() => domain.none()
        case STrue() => domain.all()
        case SAnd(l,r) => eval(mc,l) intersect eval(mc,r)
        case SOr(l,r) => eval(mc,l) union eval(mc,r)
        case SNot(phi) => eval(mc,phi).negate()
        case Satisfies(name,phi) => domain.all()
        // TODO: nc should be domain with current
        // eval(phi).equalByKey(name,"_current")
        case TypePred(name,pattern) => {
            log debug ("encountered TypePred for variable: "+name)
            domain.all().filter(x => {
                val res = types.typePred(name,pattern,x)
                log debug ("result of {} is {}",x,res)
                res
            })
        }
        case MethodPred(nameStr) => pred(mc.name.equals(nameStr))
    }

    def eval(nc:NodeCondition):ClosedEnvironment[Any] = {
		nc match {
            case False() => domain.none()
            case True() => domain.all()
            case NodePred(key) => {
                log debug("domain = ",domain.all())
                domain.all().equalByKey(key,"_current")
            }
            case Not(phi) => eval(phi).negate()
            case Or(l,r) => eval(l) union eval(r)
            case And(l,r) => eval(l) intersect eval(r)

            // Temporal cases only EX,EG,EU due to refinement
            
            // move current to predecessor
            case Exists(Next(phi)) => eval(phi).mapKey("_current",imPred)
            
            case Exists(Until(phi,psi)) => loop(eval(psi),eval(phi))
            
            case Exists(Global(phi)) =>  {
              // there must be a path where phi holds true at every step
              val phis = eval(phi)
              // initial: phis that are exit points 
              //println("GLOBAL"+phi)
              loop(phis.restrictKeyTo("_current",exit),phis)
            }
            
            case x => throw new Exception("Unknown temporal IR element: "+x)            
        }
    }
    
    /**
     * Loop to compute fixed point 
     */
    def loop(init:ClosedEnvironment[Any],iter:ClosedEnvironment[Any]) = {
      // initially init 
      var values = init
      var prev = values
      //println(iter.allValues.map(x => x("current")))
      //println(init.allValues.map(x => x("current")))
      do {
        prev = values.copy
        //println(values.allValues.map(x => x("current")))
        
        // iteratively add predecessors that are in iter
        values = values union (values.mapKey("_current",imPred) intersect iter)
      } while(prev != values)
      //println("Loop returning: "+values.allValues.map(x => x("current")))
      values
    }

}
