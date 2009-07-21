package fixbugs.mc;

/***
 * FixBugs
 * Copyright (c) 2009 Richard Warburton
 */

import java.util.HashSet;
import java.util.Set;

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
	 * Returns root node of the CFG of a given method
	 * @param owner
	 * @param mn
	 * @return
	 * @throws AnalyzerException
	 */
	public ControlFlowGraphNode getControlFlowGraph(String owner, MethodNode mn)
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
				s.successors.add(dest);
				dest.predecessors.add(s);
			}
		};
		a.analyze(owner, mn);
		ControlFlowGraphNode[] frames = (ControlFlowGraphNode[]) a.getFrames();
		for (ControlFlowGraphNode node : frames) {
			if(node.predecessors.isEmpty()) {
				 return node;
			}
		}
		
		throw new AnalyzerException("No root node");
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
