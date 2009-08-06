package fixbugs.mc

import fixbugs.core.ir.NodeCondition
import fixbugs.mc.sets._
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.AbstractInsnNode;
import scala.collection.mutable.{HashMap => MMap,Map}

/**
 * Main entry point to the bytecode analysis component
 */
object ModelCheck {

  // TODO: bytecode domain restriction
  def check(className:String,phi:NodeCondition,domain:ClosedDomain[Int]):Map[String,ClosedEnvironment[Int]] = {
    // refine IR
    val psi = Refiner.refine(phi)
    
    // extract line numbers
    val cn = new ClassNode();
	val cr = new ClassReader(className);
	cr.accept(cn, 0);
    
    // foreach method: (messy conversion from java collections)
    var results = new MMap[String,ClosedEnvironment[Int]]
    List(cn.methods).asInstanceOf[List[MethodNode]].foreach((mn) => {
        // extract cfg using asm
	    val (succs,preds) = cfg(ControlFlowGraphAnalysis.getControlFlowGraph("fixbugs",mn))
	    val lines = LineNumberExtractor.getLineNumberLookup(mn)
	    val nodes = Set() ++ lines
	    
	    // model check the method, and add the results
	    val eval:Evaluator = new Eval(nodes,domain,minimise(lines,succs),minimise(lines,preds))
	    results += (mn.name -> eval.eval(psi))
    })
    results
  }
  
  /**
   * generate lookup sets for CFG
   */
  def cfg(nodes:Array[ControlFlowGraphNode]) = {
    val succs = new MMap[Int,Set[Int]]
    val preds = new MMap[Int,Set[Int]]

    for (i <- 0 to nodes.length) {
      succs += (i -> Set(nodes(i).successors).map(nodes.indexOf(_)))
      preds += (i -> Set(nodes(i).predecessors).map(nodes.indexOf(_)))
    }
    
    (succs,preds)
  }
  
  
  /**
   * Simple Minimise Silhouettes algorithm
   * Simply substitute through the numbers and union all the appropriate sets
   */
  // TODO: remove cycles
  def minimise(lines:Array[Int],cfg:MMap[Int,Set[Int]]) = {
    cfg.transform((k,v) => v.map(x => lines(x)))
    val acc = new MMap[Int,Set[Int]]
    cfg.foreach(x => {
      val (from,to) = x
      val srcLine = lines(from)
      val toAcc = acc.getOrElse(srcLine,Set())
      acc += (srcLine -> (toAcc ++ to - srcLine))
    })
    acc
  }
  
}