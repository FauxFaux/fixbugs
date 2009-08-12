package fixbugs.util;

import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jface.text.Document;

public class EclipseUtil {

	public static CompilationUnit parse(String source) {
		Document document = new Document(source);
		ASTParser parser = ASTParser.newParser(AST.JLS3);
		parser.setSource(document.get().toCharArray());
		return (CompilationUnit) parser.createAST(null);
	}
	
}
