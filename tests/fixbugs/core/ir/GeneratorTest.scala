package fixbugs.core.ir

import org.scalatest._
import matchers._
import org.eclipse.jdt.core.dom._
import org.eclipse.jdt.core.dom.{Statement => Stmt}
import org.eclipse.jface.text.Document
import org.eclipse.text.edits._
import org.eclipse.jdt.core.dom.rewrite.ASTRewrite

class GeneratorTest extends Spec with ShouldMatchers {

  val prog =
  """
  class X {
    void a() {
      int i;
    }
  }
  """

  val parser = ASTParser.newParser(AST.JLS3)
  val (ast,orig) = {
    parser.setSource(prog.toCharArray)
    // shame you can't pattern match Java code
    val cu = parser.createAST(null).asInstanceOf[CompilationUnit]
    val classX = cu.types.get(0).asInstanceOf[TypeDeclaration]
    val a = classX.bodyDeclarations.get(0).asInstanceOf[MethodDeclaration]
    (cu.getAST,a.getBody.statements.get(0).asInstanceOf[Stmt])
  }
  val rewriter = ASTRewrite.create(ast)
  val context = Map("t" -> ast.newPrimitiveType(PrimitiveType.FLOAT))

  val gen = new ASTPatternGenerator(ast,rewriter,context)

  def replace(withNode:ASTNode) = {
    val doc = new Document(prog)
    rewriter.replace(orig,withNode,null)
    val edit = rewriter.rewriteAST(doc,null)
    edit.apply(doc)
    doc.get
  }
  
  describe("expressions") {
    println(replace(gen.generate(new Throw(new New(new SimpType("Exception"),List())))))

  }

} 
