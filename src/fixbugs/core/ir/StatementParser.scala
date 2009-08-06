package fixbugs.core.ir

import scala.util.parsing.combinator._
import scala.util.matching.Regex


/**
 * Parses Java source code pattern matching code: Expressions, Statements and Blocks
 */
object StatementParser extends RegexParsers {
  
  override def skipWhitespace = true
  def r(s:String) = regex(new Regex(s))

  def replace = r("REPLACE")
  def pwith = r("WITH")
  def pwhere = r("WHERE")
  def literal = r("[a-z0-9][a-zA-Z0-9-]*")
  def variable = r("[A-Z][a-zA-Z0-9-]*")

  def expression = literal ^^ { Literal(_) }
  
  def lb:Parser[Label] = (variable <~ ":") ~ statement ^^ { case l~s => Label(l,s) }
  def wc = r("....") ^^ { l => Wildcard() }
  def ass = variable ~ expression ^^ { case v~e => Assignment(v,e) }
  def ifelse = ("if"~>"("~>expression<~")")~block~("else"~>block) ^^ { case c~t~f => IfElse(c,t,f) }
  def loop = ("while"~>"("~>expression<~")")~block ^^ { case c~b => While(c,b) }
  def tcf = ("try"~>block)~("catch"~>block)~("finally"~>block) ^^ {case t~c~f => TryCatchFinally(t,c,f) }
  def see = expression <~ ";" ^^ { SideEffectExpr(_) }
  
  def statement =  lb | wc | ass | ifelse | loop | see 
  
  def statements:Parser[List[Product with Statement]] = statement*
  def block = "{"~>statements<~"}"  ^^ { l => SBlock(l) }

}
