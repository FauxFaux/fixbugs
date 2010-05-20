
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

import fixbugs.core.ir.SideCondition
import fixbugs.mc.sets._
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.tree.AbstractInsnNode
import scala.collection.mutable.{HashMap => MMap,Map}
import scala.collection.immutable.{Map => IMap}
import java.io.FileInputStream
import fixbugs.util.MapUtil.crossWith
import collection.jcl.MapWrapper
import org.slf4j.{Logger,LoggerFactory}

/**
 * Main entry point to the bytecode analysis component
 */
object ModelCheck {

  val log = LoggerFactory getLogger(ModelCheck getClass)

  def conMap[X,Y](jm:java.util.Map[X,Y]):IMap[X,Y] = IMap() ++ new MapWrapper[X,Y]() {
    def underlying = jm
  }

  def check(className:String,phi:SideCondition,domain:ClosedDomain[Any]):Map[String,ClosedEnvironment[Any]] = {
    // refine IR
    val psi = Refiner.refineSide(phi)
    
    // extract line numbers
    val file = new FileInputStream(className)
    val cn = new ClassNode()
	val cr = new ClassReader(file)
	cr.accept(cn, 0);

    val fieldTypes = conMap(TypeExtractor.lookupFieldTypes(cn))
    
    // foreach method: (messy conversion from java collections)
    var results = new MMap[String,ClosedEnvironment[Any]]
    for (val i <- 0 to cn.methods.size()-1) {
        val mn = cn.methods.get(i).asInstanceOf[MethodNode]
        // extract cfg using asm
	    val (succs,preds) = cfg(ControlFlowGraphAnalysis.getControlFlowGraph("fixbugs",mn))
	    val lines = LineNumberExtractor.getLineNumberLookup(mn)
        log debug ("lines = {}",lines)
        val varTypes = conMap(TypeExtractor.lookupVarTypes(mn))
	    val nodes = Set() ++ lines

        val completeDomain = domain
        
        val typeEnv = new TypeEnvironment(fieldTypes,varTypes)
	    
	    // model check the method, and add the results
	    val eval:Evaluator = new Eval(typeEnv,nodes,completeDomain,succs,preds) //minimise(lines,succs),minimise(lines,preds)
        log debug ("calling eval for method: {} with types {}",mn.name,typeEnv)
	    results += (mn.name -> eval.eval(mn,psi))
    }
    results
  }
 
  def convert[X](from:java.util.Set[X]):Set[X] = {
    var s = Set[X]()
    if(from != null) {
        val it = from.iterator
        while(it.hasNext)
            s = s + it.next
    }
    s
  }

  /**
   * generate lookup sets for CFG
   */
  def cfg(nodes:Array[ControlFlowGraphNode]) = {
    val succs = new MMap[Int,Set[Int]]
    val preds = new MMap[Int,Set[Int]]

    for (i <- 0 to nodes.length-1) {
      // TODO: check this
      if(nodes(i) != null) {
          succs += (i -> convert(nodes(i).successors).map(nodes.indexOf(_)))
          preds += (i -> convert(nodes(i).predecessors).map(nodes.indexOf(_)))
      }
    }
   
    log debug ("succs = {}",succs)
    log debug ("preds = {}",succs)
    (succs,preds)
  }
  
  /**
   * Simple Minimise Silhouettes algorithm
   * Simply substitute through the numbers and union all the appropriate sets
   */
  // TODO: remove cycles
  def minimise(lines:Array[Int],cfg:MMap[Int,Set[Int]]) = {
    cfg.transform((k,v) => v.map(x => lines(x)))
    log debug ("transformed cfg = {}",cfg)
    val acc = new MMap[Int,Set[Int]]
    cfg.foreach(x => {
      val (from,to) = x
      val srcLine = lines(from)
      val toAcc = acc.getOrElse(srcLine,Set())
      acc += (srcLine -> (toAcc ++ to - srcLine))
    })
    log debug ("minimised acc = {}",cfg)
    acc
  }
  
}
