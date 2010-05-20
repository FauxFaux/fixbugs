
/**
 *
 * This file is part of Fixbugs.
 * 
 * Fixbugs is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Fixbugs is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with Fixbugs.  If not, see <http://www.gnu.org/licenses/>.
 *
 **/

package fixbugs.mc

import fixbugs.core.ir._
import fixbugs.mc.sets._
import scala.collection.mutable.{Map => MMap}
import org.slf4j.{Logger,LoggerFactory}
import org.objectweb.asm.tree.MethodNode
import fixbugs.util.MapUtil.crossWith

case class Node(lineNumber:Int){}

trait Evaluator {
  def eval(mc:MethodNode,nc:SideCondition):ClosedEnvironment[Any]
}

class Eval(typeEnv:TypeEnvironment,iNodes:Set[Int],dom:ClosedDomain[Any],succ:MMap[Int,Set[Int]],pred:MMap[Int,Set[Int]]) extends Evaluator {

    val log = LoggerFactory getLogger(this getClass)

	val imPred = (Map() ++ pred).asInstanceOf[Map[Any,Set[Any]]]
	val imSucc = (Map() ++ succ).asInstanceOf[Map[Any,Set[Any]]]
    val nodes = iNodes.asInstanceOf[Set[Any]]
    val types = new TypeComparator(typeEnv)
	
	// entry points are nodes with no predecessors
	val entry = nodes -- imPred.keys
	
	// exit points are nodes with no successors
	val exit = nodes -- imSucc.keys

    var domain = dom

    // converts boolean predicate to domains
    def pred(p:Boolean) = if (p) domain.all() else domain.none()
    
    var tabIndent:Int = 0

    def tabs() = (0 to tabIndent).foldLeft(new StringBuilder){(acc,_) => acc.append("\t")} toString

    // printable string representation of a result
    def strRes(res:ClosedEnvironment[Any]) = (Map() ++ res.allValues).foldLeft(""){(x,acc)=>acc+"\n"+tabs+x}

    def eval(mc:MethodNode,sc:SideCondition):ClosedEnvironment[Any] = sc match {
        case SFalse() => domain.none()
        case STrue() => domain.all()
        case SAnd(l,r) => eval(mc,l) intersect eval(mc,r)
        case SOr(l,r) => eval(mc,l) union eval(mc,r)
        case SNot(phi) => eval(mc,phi).negate()
        case Satisfies(name,phi) => {
            val oldDomain = domain
            domain = new SetClosedDomain[Any](crossWith(domain.allValues,"_current",nodes))
            val res = eval(phi)
            domain = oldDomain
            log debug ("satisifes, name = {}, res no check = {}",name,strRes(res))
            res.equalByKey(name,"_current")
        }
        // TODO: nc should be domain with current
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
        tabIndent += 1
		val matchRes = nc match {
            case False() => domain.none()
            case True() => domain.all()
            case NodePred(key) => {
                //log debug("domain = ",domain.all())
                val res = domain.all().equalByKey(key,"_current")
                log debug (tabs + "NodePred, key = {}, res = {}",key,strRes(res))
                res
            }
            case Not(phi) => eval(phi).negate()
            case Or(l,r) => eval(l) union eval(r)
            case And(l,r) => eval(l) intersect eval(r)

            // Temporal cases only EX,EG,EU due to refinement
            
            // move current to predecessor
            case Exists(Next(phi)) => {
                val phiRes = eval(phi)
                log debug (tabs + "EX - phi {},phiRes = {}, imPred = "+imPred,phi,strRes(phiRes))
                val res = phiRes.mapKey("_current",imPred)
                log debug (tabs + "EX - res = {}",strRes(res))
                res
            }
            
            case Exists(Until(phi,psi)) => loop(eval(psi),eval(phi))
            
            case Exists(Global(phi)) =>  {
              // there must be a path where phi holds true at every step
              val phis = eval(phi)
              // initial: phis that are exit points 
              loop(phis.restrictKeyTo("_current",exit),phis)
            }
            //case x => throw new Exception("Unknown temporal IR element: "+x)            
        }
        tabIndent -= 1
        matchRes
    }
    
    /**
     * Loop to compute fixed point 
     */
    def loop(init:ClosedEnvironment[Any],iter:ClosedEnvironment[Any]) = {
      // initially init 
      var values = init
      var prev = values
      do {
        prev = values.copy
        
        // iteratively add predecessors that are in iter
        values = values union (values.mapKey("_current",imPred) intersect iter)
      } while(prev != values)
      values
    }

}
