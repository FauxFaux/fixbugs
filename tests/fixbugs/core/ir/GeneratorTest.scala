package fixbugs.core.ir

import org.scalatest._
import matchers._
import org.eclipse.jdt.core.dom._
import org.eclipse.jdt.core.dom.{Statement => Stmt}
import org.eclipse.jface.text.Document
import org.eclipse.text.edits._
import org.eclipse.jdt.core.dom.rewrite.ASTRewrite
import org.eclipse.jdt.core.dom.InfixExpression.Operator._
import org.eclipse.jdt.core.dom.PostfixExpression.Operator._

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
  val context = Map(
    "t" -> ast.newPrimitiveType(PrimitiveType.FLOAT),
    "1" -> ast.newNumberLiteral("1"),
    "2" -> ast.newNumberLiteral("2"),
    "x" -> ast.newName("x")
  )

  val gen = new ASTPatternGenerator(ast,rewriter,context)

  def replace(withNode:ASTNode) = {
    val doc = new Document(prog)
    rewriter.replace(orig,withNode,null)
    val edit = rewriter.rewriteAST(doc,null)
    edit.apply(doc)
    doc.get
  }

  def ignoreEq(a:String,b:String) {
    val (as,bs) = (lines(a),lines(b))
    assert(as.length == bs.length)
    for( (aline,bline) <- (as zip bs)) {
        eq(aline,bline)
    }
  }

  def eq(a:String,b:String) = {
    try {
          assert(a.trim == b.trim)
    } catch {
        case ex => {
            ex.printStackTrace
            println(ex)
            println(a)
        }
    }
  }

  def lines(s:String) = s.split("\\n")
  
  describe("expressions") {
    def check(a:String,b:String) = eq(lines(a)(3),b)
    def checkEx(e:Expression,b:String) = check(replace(gen.generate(new SideEffectExpr(e))),b)
   
    check(replace(gen.generate(new Throw(new New(new SimpType("Exception"),List())))),"throw new Exception();")
    checkEx(BinOp(Metavar("1"),Metavar("2"),PLUS),"1 + 2;")
    checkEx(UnOp(Metavar("x"),INCREMENT),"x++;")
    checkEx(Cast(Metavar("1"),PrimType("float")),"(float)1;")
    checkEx(New(SimpType("Object"),List()),"new Object();")
  }

} 
