package fixbugs.mc

import fixbugs.core.ir.SideCondition
import fixbugs.mc.sets._
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.AbstractInsnNode;
import scala.collection.mutable.{HashMap => MMap,Map}
import java.io.FileInputStream
import fixbugs.util.MapUtil.crossWith

/**
 * Main entry point to the bytecode analysis component
 */
object ModelCheck {

  def check(className:String,phi:SideCondition,domain:ClosedDomain[Int]):Map[String,ClosedEnvironment[Int]] = {
    // refine IR
    val psi = Refiner.refineSide(phi)
    
    // extract line numbers
    //println(className);
    val file = new FileInputStream(className)
    val cn = new ClassNode()
	val cr = new ClassReader(file)
	cr.accept(cn, 0);

    val fieldTypes = TypeExtractor.lookupFieldTypes(cn)
    
    // foreach method: (messy conversion from java collections)
    var results = new MMap[String,ClosedEnvironment[Int]]
    for(val i <- 0 to cn.methods.size()-1) {
        val mn = cn.methods.get(i).asInstanceOf[MethodNode]
        // extract cfg using asm
	    val (succs,preds) = cfg(ControlFlowGraphAnalysis.getControlFlowGraph("fixbugs",mn))
	    val lines = LineNumberExtractor.getLineNumberLookup(mn)
        val varTypes = TypeExtractor.lookupVarTypes(mn)
	    val nodes = Set() ++ lines

        // cross product the domain with the current value
        // TODO: fix
        val completeDomain = domain //new SetClosedDomain[Int](crossWith(domain.allValues,"_current",nodes))
	    
	    // model check the method, and add the results
	    val eval:Evaluator = new Eval(nodes,completeDomain,minimise(lines,succs),minimise(lines,preds))
	    results += (mn.name -> eval.eval(psi))
    }
    results
  }
 
  def convert[X](from:java.util.Set[X]):Set[X] = {
    var s = Set[X]()
    val it = from.iterator
    while(it.hasNext)
        s = s + it.next
    s
  }

  /**
   * generate lookup sets for CFG
   */
  def cfg(nodes:Array[ControlFlowGraphNode]) = {
    val succs = new MMap[Int,Set[Int]]
    val preds = new MMap[Int,Set[Int]]

    /*for (i <- 0 to nodes.length-1) {
        println(nodes(i).successors)
        val it = nodes(i).successors.iterator
        while(it.hasNext()) {
            println(nodes.indexOf(it.next()))
        }
    }*/

    for (i <- 0 to nodes.length-1) {
      succs += (i -> convert(nodes(i).successors).map(nodes.indexOf(_)))
      preds += (i -> convert(nodes(i).predecessors).map(nodes.indexOf(_)))
    }
    
    //printf("cfg = %s\n",(succs,preds))
    (succs,preds)
  }
  
  /**
   * Simple Minimise Silhouettes algorithm
   * Simply substitute through the numbers and union all the appropriate sets
   */
  // TODO: remove cycles
  def minimise(lines:Array[Int],cfg:MMap[Int,Set[Int]]) = {
    //println(cfg)
    cfg.transform((k,v) => v.map(x => lines(x)))
    val acc = new MMap[Int,Set[Int]]
    cfg.foreach(x => {
      val (from,to) = x
      val srcLine = lines(from)
      val toAcc = acc.getOrElse(srcLine,Set())
      acc += (srcLine -> (toAcc ++ to - srcLine))
    })
    //printf("acc = %s\n",acc)
    acc
  }
  
}
