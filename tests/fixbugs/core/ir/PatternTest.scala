package fixbugs.core.ir

import org.scalatest._
import matchers._
import org.eclipse.jdt.core.dom.InfixExpression.Operator._
import org.eclipse.jdt.core.dom.PostfixExpression.Operator._

import Statement._

class PatternTest extends Spec with ShouldMatchers {

  val simple1 = """
  class X {
	  private final int y = 4;
	  public void test() {
		  int x = 1 + 2;
		  System.out.println(this.y);
	  }
  }
  """
  
  val simple2 = """
  class X {
	  private final int y = 4;
	  public void test() {
		  int y = 2;
		  y--;
		  int x = 1 + y;
		  System.out.println(this.y);
	  }
  }
  """
  
  val conditional = """
  class X {
	  public void test() {
		  if(true)
			  System.out.println("hello world");
		  int i = 0;
		  while(i < 5) {
			System.out.println("hello world");
			i++;
	      }
	  }
  }
  """

  val matcher = new ASTPatternMatcher
  
  def unify(src:String,pat:Statement) = matcher.unifyAll(src,pat).toList
  def s1(pat:Statement) = unify(simple1,pat)
  def s2(pat:Statement) = unify(simple2,pat)
  def cond(pat:Statement) = unify(conditional,pat)
  
  describe("assignments and labels") {
    val xy = s1(new Assignment("x",Metavar("y")))
    println("xy = " + xy)
    xy should have length 1
    val all = s1(Label("all",Wildcard()))
    println("printer = " + all)
    all should have length (2)
  }
  
  describe ("expressions") {
    val x = s2(Assignment("x",BinOp(Metavar("1"),Metavar("y"),PLUS)))
    x should have length 1
    println("x = "+x)
    val y = s2(SideEffectExpr(UnOp(Metavar("y"),DECREMENT)))
    y should have length 1
    println("y = "+y)
  }
  
  describe("methods in expressions") {
    val printer = s1(SideEffectExpr(Method("func",List(Metavar("arg1")))))
    println("printer = "+ printer)
    printer should have length (1)
    val noargs = s1(SideEffectExpr(Method("func",Nil)))
    println("noargs = "+ noargs)
    noargs should have length (1)
  }
  
  describe("if and while") {
    val iff = cond(IfElse(Metavar("cond"),WildLabel("then"),Wildcard()))
    println("iff = "+iff)
    iff should have length 1
    
    val whiel = cond(While(Metavar("cond"),WildLabel("body")))
    println("whiel = "+whiel)
    whiel should have length 1
  }
  
}
