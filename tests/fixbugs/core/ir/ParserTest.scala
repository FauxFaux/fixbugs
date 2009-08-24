package fixbugs.core.ir

import org.scalatest._
import matchers._
import Parser._
import org.eclipse.jdt.core.dom.PostfixExpression.{Operator => PostFixOp}
import org.eclipse.jdt.core.dom.InfixExpression.{Operator => InfixOp}

class ParserTest extends Spec with ShouldMatchers {

  def p[X](m:Parser[X],s:String,x:X) = {
  m.apply(new lexical.Scanner(s)) match {
    case Success(ord, _) => ord should equal (x)
    case Failure(msg, _) => println(s,msg)
    case Error(msg, _) => println(s,msg)
  }
  }

  describe("expresssions") {
    p(unary,"i",Metavar("i"))
    p(unary,"i++",UnOp(Metavar("i"),PostFixOp.INCREMENT))
    p(expression,"i+j",BinOp(Metavar("i"),Metavar("j"),InfixOp.PLUS))
  }

  describe("statements") {
    p(statement,"i=j;",Assignment("i",Metavar("j")))
    p(statement,"L: ....",Label("L",Wildcard()))
    p(statement,"if(m) .... else ....",IfElse(Metavar("m"),Wildcard(),Wildcard()))
    p(statement,"while(m) ....",While(Metavar("m"),Wildcard()))
    p(statement,"return m;",Return(Metavar("m")))
    p(statement,"m;",SideEffectExpr(Metavar("m")))
    p(statement,"{m;....}",SBlock(List(SideEffectExpr(Metavar("m")),Wildcard())))

  }
  
  describe("node conditions") {
    p(_unary,"true",True())
    p(_unary,"false",False())
    p(_unary,"¬ false",Not(False()))
    p(node,"true ^ false",And(True(),False()))
    p(node,"true | false",Or(True(),False()))
    p(node,"true ^ (false | true)",And(True(),Or(False(),True())))
    p(node,"¬ true ^ (false | true)",And(Not(True()),Or(False(),True())))
    p(node,"true ^ (false | ¬ true)",And(True(),Or(False(),Not(True()))))
    p(node,"E[X is m]",Exists(Next(NodePred("m"))))
    p(node,"A[false U true] ^ E[X is m]",And(All(Until(False(),True())),Exists(Next(NodePred("m")))))
  }

  describe("side conditions") { 
    p(side,"True",STrue())
    p(side,"False",SFalse())
    p(side,"False and True",SAnd(SFalse(),STrue()))
    p(side,"(not False) or True",SOr(SNot(SFalse()),STrue()))
    p(side,"True and False or not True",SAnd(STrue(),SOr(SFalse(),SNot(STrue()))))
  }

}
