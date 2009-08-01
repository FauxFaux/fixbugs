package fixbugs.mc;

import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.LineNumberNode;
import org.objectweb.asm.tree.LocalVariableNode;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.VarInsnNode;

public class LineNumberExtractor {

	/**
	 * @param args
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public static Map<AbstractInsnNode, Integer> getLineNumbers(ClassNode cn) {
		final List<MethodNode> methods = (List<MethodNode>) cn.methods;
		final Map<AbstractInsnNode, Integer> lineNumbers = new java.util.HashMap<AbstractInsnNode, Integer>();
		int currentLineNumber = -1;
		for (final MethodNode method : methods) {
			//method.localVariables
			final ListIterator it = method.instructions.iterator();
			while (it.hasNext()) {
				final AbstractInsnNode n = (AbstractInsnNode) it.next();
				if (n instanceof LineNumberNode) {
					final LineNumberNode lnn = (LineNumberNode) n;
					currentLineNumber = lnn.line;
				} else {
					/*if (n instanceof VarInsnNode) {
						VarInsnNode varNode = (VarInsnNode) n;
						if(varNode.getOpcode() == ISTORE) {
							System.out.println(varName(method,varNode));
						}
					}*/
					lineNumbers.put(n, currentLineNumber);
				}
			}
		}
		return lineNumbers;
	}
	
	public static String varName(final MethodNode method, final VarInsnNode varNode) {
		LocalVariableNode lvn = (LocalVariableNode) method.localVariables.get(varNode.var);
		System.out.println(lvn.desc);
		return lvn.name;
	}
	
	public static void main(String[] args) throws Exception  {
		final String className = "fixbugs.test.Simple";
		final ClassNode cn = new ClassNode();
		final ClassReader cr = new ClassReader(className);
		cr.accept(cn, 0);
		
		System.out.println(getLineNumbers(cn));
	}
	
}