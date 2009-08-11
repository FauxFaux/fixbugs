package fixbugs.core.ir
import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

/**
 * Parses Fixbugs transformations
 */
object Parser extends RegexParsers {

  // Common
  override def skipWhitespace = true
  def r(s:String) = regex(new Regex(s))
  def literal = r("[a-z0-9][a-zA-Z0-9-]*")
  def variable = r("[A-Z][a-zA-Z0-9-]*")
  
  def o(op:Parser[String],cons:Operator):Parser[Operator] = op ^^ {v => cons}
  
  def operator = o("+",new+)|o("-",new-)|o("*",new*)|o("/",new/)|
    o("%",new%)|o("&",new&)|o("&&",new&&)|o("|",new|)|o("||",new||)|o("!",new!)
  
  // Statement parsing
  def lit = literal ~ opt("("~> (expression*) <~ ")") ^^ (x => x match {
    case l~None => Metavar(l)
    case l~Some(e) => Method(l,e)
  })
  
  def un = operator ~ expression ^^ {case o~e => UnOp(e,o)} | lit
  def expression:Parser[Expression] = (un ~ opt(operator ~ expression)) ^^ (x => x match { 
  	case u~None => u
  	case u~Some(op~ex) => BinOp(u,ex,op)
  })
  
  def lb:Parser[Label] = (variable <~ ":") ~ statement ^^ { case l~s => Label(l,s) }
  def wc = r("....") ^^ { l => Wildcard() }
  def ass = variable ~ expression ^^ { case v~e => Assignment(v,e) }
  def ifelse = ("if"~>"("~>expression<~")")~block~("else"~>block) ^^ { case c~t~f => IfElse(c,t,f) }
  def loop = ("while"~>"("~>expression<~")")~block ^^ { case c~b => While(c,b) }
  def tcf = ("try"~>block)~("catch"~>block)~("finally"~>block) ^^ {case t~c~f => TryCatchFinally(t,c,f) }
  def see = expression <~ ";" ^^ { SideEffectExpr(_) }
  
  def statement =  lb | wc | ass | ifelse | loop | see 
  
  def statements:Parser[List[Product with Statement]] = statement*
  def block = "{"~>statements<~"}"  ^^ { SBlock(_) }
  def sblock = statements ^^ { SBlock(_) }
  
  // Side Condition Matching
  def future = "F" ~> node ^^ (Future(_))
  def global = "G" ~> node ^^ (Global(_))
  def next = "X" ~> node ^^ (Next(_))
  def until = node ~ "U" ~ node ^^ {case l~_~r => Until(l,r)}
  
  def path = future | global | next | until
  
  def _true = "true" ^^ (_=>True())
  def _false = "false" ^^ (_=>False())
  def and = node ~ "^" ~ node ^^ {case l~_~r => And(l,r)}
  def or = node ~ "|" ~ node ^^ {case l~_~r => Or(l,r)}
  def not = "¬" ~ node ^^ {case _~n => Not(n)}
  def brackets= "("~>node<~")"
  def pred = "is"~>variable ^^ (NodePred(_))
  def all = "A[" ~> path <~"]" ^^ (All(_))
  def ex = "E[" ~> path <~ "]" ^^ (Exists(_))
  
  def node:Parser[NodeCondition] =
	 _true | _false | and | or | not | brackets | pred | all | ex
  
  def sat = ("{"~>node<~"}") ~ "@" ~ variable ^^ {case n~_~v => Satisfies(v,n)}
  def sand = side ~ "and" ~ side ^^ {case l~_~r => SAnd(l,r)}
  def sor = side ~ "or" ~ side ^^ {case l~_~r => SOr(l,r)}
  def snot = "not" ~> side ^^ {case s => SNot(s)}
  
  def side:Parser[SideCondition] =  sat | sand | sor | snot
  
  // Transformations
  def replace = ("REPLACE"~> sblock)~ ("WITH" ~> sblock) ~ ("WHERE" ~> side) ^^ {
    case from~to~cond => Replace(from,to,cond)
  }
  def then = (trans <~ "then") ~ trans ^^ {case l~r => Then(l,r)}
  
  def trans:Parser[Transformation] = replace | then
  
}
