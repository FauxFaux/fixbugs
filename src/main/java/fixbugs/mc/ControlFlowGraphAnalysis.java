package fixbugs.mc;

/***
 * FixBugs
 * Copyright (c) 2009 Richard Warburton
 */

import java.util.HashSet;
import java.util.Set;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.analysis.Analyzer;
import org.objectweb.asm.tree.analysis.AnalyzerException;
import org.objectweb.asm.tree.analysis.BasicInterpreter;
import org.objectweb.asm.tree.analysis.Frame;

/**
 * Generates Control Flow Graph
 * 
 * @author Richard Warburton
 */
public class ControlFlowGraphAnalysis {
	
	/**
	 * Test main method
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		final String className = "fixbugs.test.Simple";
		final ClassNode cn = new ClassNode();
		final ClassReader cr = new ClassReader(className);
		cr.accept(cn, 0);
		
		MethodNode mn = (MethodNode) cn.methods.get(1);
		final ControlFlowGraphNode[] root = getControlFlowGraph("foo", mn);
		for (ControlFlowGraphNode frame: root) {
			if(frame != null) {
				System.out.println(frame);
				System.out.println(frame.predecessors.size());
				System.out.println(frame.successors.size());
			}
		}
	}
	
	
	/**
	 * Returns root node of the CFG of a given method
	 * @param owner
	 * @param mn
	 * @return
	 * @throws AnalyzerException
	 */
	public static ControlFlowGraphNode[] getControlFlowGraph(String owner, MethodNode mn)
			throws AnalyzerException {

		Analyzer a = new Analyzer(new BasicInterpreter()) {

			protected Frame newFrame(int nLocals, int nStack) {
				return new ControlFlowGraphNode(nLocals, nStack);
			}

			protected Frame newFrame(Frame src) {
				return new ControlFlowGraphNode(src);
			}

			protected void newControlFlowEdge(int src, int dst) {
				ControlFlowGraphNode s = (ControlFlowGraphNode) getFrames()[src];
				ControlFlowGraphNode dest = (ControlFlowGraphNode) getFrames()[dst];
                // TODO: check exceptional nodes
                if(s != null && dest != null) {
    				s.successors.add(dest);
	    			dest.predecessors.add(s);
                }
			}
			
			@Override
			protected boolean newControlFlowExceptionEdge(int src, int dst) {
				// TODO: decide how to explicitly represent this information
				newControlFlowEdge(src, dst);
				return super.newControlFlowExceptionEdge(src, dst);
			}
		};
		a.analyze(owner, mn);
		Frame[] frames = a.getFrames();
		ControlFlowGraphNode[] nodes = new ControlFlowGraphNode[frames.length];
		for (int i = 0; i < frames.length; i++) {
			nodes[i] = (ControlFlowGraphNode) frames[i];
		}
		return nodes;
	}
}

class ControlFlowGraphNode extends Frame {

	final Set<ControlFlowGraphNode> successors = new HashSet<ControlFlowGraphNode>();
	final Set<ControlFlowGraphNode> predecessors = new HashSet<ControlFlowGraphNode>();

	public ControlFlowGraphNode(int nLocals, int nStack) {
		super(nLocals, nStack);
	}

	public ControlFlowGraphNode(Frame src) {
		super(src);
	}
}
