package test

import org.eclipse.jdt.core.dom._
import org.eclipse.jdt.core.dom.ASTParser
import org.eclipse.jdt.core.dom.CompilationUnit
import org.eclipse.jdt.core.dom.ImportDeclaration
import org.eclipse.jdt.core.dom.TypeDeclaration
import org.eclipse.jdt.core.dom.rewrite.ASTRewrite
import org.eclipse.jdt.core.dom.rewrite.ITrackedNodePosition
import org.eclipse.jdt.core.dom.rewrite.ListRewrite
import org.eclipse.jface.text.BadLocationException
import org.eclipse.jface.text.Document
import org.eclipse.text.edits.MalformedTreeException
import org.eclipse.text.edits.TextEdit
import org.eclipse.text.edits.UndoEdit
import org.eclipse.jdt.core.dom.ASTVisitor;

object ApiTest {
  
  def before = """
  class X {
	  private final int y = 4;
	  public void test() {
		  int x = 1 + 2;
		  System.out.println(this.y);
	  }
  }
  """
  
  def main(args : Array[String]) : Unit = {
		//applyVisitor(null,before);
		val document = new Document(before);
		val  parser = ASTParser.newParser(AST.JLS3);
		parser.setSource(document.get().toCharArray());
		val cu = parser.createAST(null).asInstanceOf[CompilationUnit];
		val X = cu.types.get(0).asInstanceOf[TypeDeclaration]
		val test = X.getMethods.apply(0)
		val stmts = test.getBody.statements.asInstanceOf[java.util.List[Statement]]
		val assign = stmts.get(0).asInstanceOf[VariableDeclarationStatement]
        val syso = stmts.get(1).asInstanceOf[ExpressionStatement].getExpression.asInstanceOf[MethodInvocation]
        println(syso.getExpression)
        println(syso.arguments.get(0))
        println(syso.arguments.get(0).asInstanceOf[Object].getClass)
        //println(syso.getClass)
        
		println("hai")
		/*val ast = cu.getAST();
		val id = ast.newImportDeclaration();
		id.setName(ast.newName(Array("java", "util", "Set")));
		val rewriter = ASTRewrite.create(ast);
		val td = cu.types().get(0).asInstanceOf[TypeDeclaration];
		val tdLocation = rewriter.track(td);
		val lrw = rewriter.getListRewrite(cu,CompilationUnit.IMPORTS_PROPERTY);
		lrw.insertLast(id, null);
		val edits = rewriter.rewriteAST(document, null);
		try {
			val undo = edits.apply(document);
		} catch {
		  case e => e.printStackTrace()
		}
		println(document.get());*/
		//println(tdLocation.getStartPosition()+" ; "+tdLocation.getLength());
		// are new source range for "class X {}" in document.get()
  }
  
  def applyVisitor(visitor:ASTVisitor,program:String) = {
    val document = new Document(program);
	val parser = ASTParser.newParser(AST.JLS3);
	parser.setSource(document.get().toCharArray());
	val cu = parser.createAST(null).asInstanceOf[CompilationUnit];
	cu.accept(visitor);
  }
}