package fixbugs.core.ir
import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

import org.eclipse.jdt.core.dom.InfixExpression.{Operator => Op}
import org.eclipse.jdt.core.dom.PostfixExpression.{Operator => PostOp}

/**
 * Parses Fixbugs transformations
 */
object Parser extends RegexParsers {

  // Common
  override def skipWhitespace = true
  def r(s:String) = regex(new Regex(s))
  def literal = r("[a-z0-9][a-zA-Z0-9-]*")
  def variable = r("[A-Z][a-zA-Z0-9-]*")
  
  val operators = List(
    Op.AND,
    Op.CONDITIONAL_AND,
    Op.CONDITIONAL_OR,
    Op.DIVIDE,
    Op.EQUALS,
    Op.GREATER,
    Op.GREATER_EQUALS,
    Op.LEFT_SHIFT,
    Op.LESS,
    Op.LESS_EQUALS,
    Op.MINUS,
    Op.NOT_EQUALS,
    Op.OR,
    Op.PLUS,
    Op.REMAINDER,
    Op.RIGHT_SHIFT_SIGNED,
    Op.RIGHT_SHIFT_UNSIGNED,
    Op.TIMES,
    Op.XOR,
    Op.OR)
  
  val postfix = List(PostOp.DECREMENT, PostOp.INCREMENT)

  // No code reuse because of lack of common supertype
  // TODO: try structural type refinements
  def infixOperator:Parser[Op] = {
    def parseOp(op:Op):Parser[Op] = literal(op.toString) ^^ Op.toOperator
    operators.tail.map(parseOp).foldLeft(parseOp(operators.head))(_|_)
  }
  
  def postfixOperator:Parser[PostOp] = {
    def parseOp(op:PostOp):Parser[PostOp] = literal(op.toString) ^^ PostOp.toOperator
    postfix.tail.map(parseOp).foldLeft(parseOp(postfix.head))(_|_)
  }
  
  // Statement parsing
  def lit = literal ~ opt("("~> (expression*) <~ ")") ^^ (x => x match {
    case l~None => Metavar(l)
    case l~Some(e) => Method(l,e)
  })
  
  def un = expression ~ postfixOperator ^^ {case e~o => UnOp(e,o)} | lit
  def expression:Parser[Expression] = (un ~ opt(infixOperator ~ expression)) ^^ (x => x match { 
  	case u~None => u
  	case u~Some(op~ex) => BinOp(u,ex,op)
  })
 
  // TODO: check the scala docs for pro way to do this
  def exprs = expression
 
  def lb:Parser[Label] = (variable <~ ":") ~ statement ^^ { case l~s => Label(l,s) }
  def wc = r("....") ^^ { l => Wildcard() }
  def ass = variable ~ expression ^^ { case v~e => Assignment(v,e) }
  def ifelse = ("if"~>"("~>expression<~")")~block~("else"~>block) ^^ { case c~t~f => IfElse(c,t,f) }
  def loop = ("while"~>"("~>expression<~")")~block ^^ { case c~b => While(c,b) }
  def tcf = ("try"~>block)~("catch"~>block)~("finally"~>block) ^^ {case t~c~f => TryCatchFinally(t,c,f) }
  def see = expression <~ ";" ^^ { SideEffectExpr(_) }
  def block = "{" ~> statements <~ "}" ^^ { SBlock(_)}
  def returnStmt = "return" ~> expression <~ ";" ^^ {Return(_)}
  def throww = "throw" ~> expression <~ ":" ^^ {Throw(_)}
  def fors = "for" ~> "(" ~> (forLoop ) //| foreach)
  def forLoop = expression ~ (";" ~> expression) ~ (";" ~> expression) ~ statement ^^ {
    case init~cond~updaters~stmt => For(List(init),cond,List(updaters),stmt)
  }
  def foreach = "TODO"
  def doLoop = ("do" ~> statement) ~ ("while" ~> "(" ~> expression <~ ")" <~ ";") ^^ {
    case s~e => Do(s,e)
  }
  def sync = ("synchronized" ~> "(" ~> expression <~ ")") ~ block ^^ {
    case e~b => Synchronized(b,e)
  }
  def switch = ("switch" ~> "(" ~> expression <~ ")") ~ ("{" ~> statements <~ "}") ^^ {
    case e~s => Switch(s,e)
  }
  def default = "default" ~ ";" ^^ { _ => DefaultCase() }
  def switchcase = "case" ~> expression <~ ":" ^^ { Switchcase(_) }
  def break = "break" ~> literal <~ ";" ^^ { Break(_) }
  def continue = "continue" ~> literal <~ ";" ^^ { Continue(_) }
  def assert = "assert" ~> expression <~ ";" ^^ { Assert(_) }
  def cons = "this" ~> "(" ~> exprs <~ ")" ^^ {Constructor(List(_:Expression))}
  def scons = "super" ~> "(" ~> exprs <~ ")" ^^ {SuperConstructor(List(_:Expression))}

  def statement:Parser[Statement] =
    lb | wc | ass | ifelse | loop | see | doLoop | 
    sync | switch | default | switchcase | 
    break | continue | assert | fors | cons | scons
  
  def statements:Parser[List[Statement]] = statement*
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

