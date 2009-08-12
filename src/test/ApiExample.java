package test;

import java.util.List;

import org.eclipse.jdt.core.dom.*;
import org.eclipse.jdt.core.dom.InfixExpression.Operator;
import org.eclipse.jface.text.Document;
import org.eclipse.text.edits.MalformedTreeException;
import org.eclipse.text.edits.TextEdit;
import org.eclipse.text.edits.UndoEdit;

import static java.lang.System.out;

public class ApiExample {

	/**
	 * @param args
	 */
	public static void main(String... args) {
		
		System.out.println(Operator.AND);
		System.out.println(Operator.toOperator("fooo"));
		
		Document document = new Document("import java.util.List;\nclass X {}\n");
		ASTParser parser = ASTParser.newParser(AST.JLS3);
		parser.setSource(document.get().toCharArray());
		CompilationUnit cu = (CompilationUnit) parser.createAST(null);
		final List types = cu.types();
		System.out.println(types.size());
		final TypeDeclaration X = (TypeDeclaration) types.get(0);
		
		System.out.println(X.getClass());
		
		final AST ast = cu.getAST();
		LabeledStatement s = ast.newLabeledStatement();
		//final InfixExpression infix = ast.newInfixExpression();
		//infix.setOperator(Operator.AND.)
	
		
		
		/*AST ast = cu.getAST();
		ImportDeclaration id = ast.newImportDeclaration();
		id.setName(ast.newName(new String[] { "java", "util", "Set" }));
		ASTRewrite rewriter = ASTRewrite.create(ast);
		TypeDeclaration td = (TypeDeclaration) cu.types().get(0);
		ITrackedNodePosition tdLocation = rewriter.track(td);
		ListRewrite lrw = rewriter.getListRewrite(cu,
				CompilationUnit.IMPORTS_PROPERTY);
		lrw.insertLast(id, null);
		TextEdit edits = rewriter.rewriteAST(document, null);
		UndoEdit undo = null;
		try {
			undo = edits.apply(document);
		} catch (MalformedTreeException e) {
			e.printStackTrace();
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		out.println("import java.util.List;\nimport java.util.Set;\nclass X {}\n"
						.equals(document.get()));
		out.println(tdLocation.getStartPosition() + " ; "
				+ tdLocation.getLength());
		// are new source range for "class X {}" in document.get()
		try {
			undo.apply(document);
		} catch (MalformedTreeException e) {
			e.printStackTrace();
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
		System.out.println(document.get());
*/
	}

}
