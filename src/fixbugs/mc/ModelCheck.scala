package fixbugs.mc

import fixbugs.mc.ir.NodeCondition
import fixbugs.mc.sets._
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.AbstractInsnNode;
import scala.collection.mutable.{HashMap,Map}

/**
 * Main entry point to the bytecode analysis component
 */
object ModelCheck {

  // TODO: calculate domain
  def check(className:String,phi:NodeCondition,domain:ClosedDomain[Int]):Map[String,ClosedEnvironment[Int]] = {
    // refine IR
    val psi = Refiner.refine(phi)
    
    // extract line numbers
    val cn = new ClassNode();
	val cr = new ClassReader(className);
	cr.accept(cn, 0);
    val lines = LineNumberExtractor.getLineNumbers(cn)
    
    // foreach method: (messy conversion from java collections)
    
    var results = new HashMap[String,ClosedEnvironment[Int]]
    List(cn.methods).asInstanceOf[List[MethodNode]].foreach((mn) => {
        // extract cfg using asm
	    val (succs,preds) = cfg(ControlFlowGraphAnalysis.getControlFlowGraph("fixbugs",mn))
	    
	    // model check the method, and add the results
	    val eval:Evaluator = new Eval(domain,succs,preds)
	    results += (mn.name -> eval.eval(psi))
    })
    results
  }
  
  /**
   * generate lookup sets for CFG
   */
  def cfg(nodes:Array[ControlFlowGraphNode]) = {
    val succs = new HashMap[Int,Set[Int]]
    val preds = new HashMap[Int,Set[Int]]

    // need indices :. fail loop
    var i = 0
    while(i < nodes.length) {
      succs += (i -> Set(nodes(i).successors).map(nodes.indexOf(_)))
      preds += (i -> Set(nodes(i).predecessors).map(nodes.indexOf(_)))
      i += 1
    }
    
    (succs,preds)
  }
  
}
