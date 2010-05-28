
/**
 *
 * This file is part of Fixbugs.
 * 
 * Fixbugs is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Fixbugs is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with Fixbugs.  If not, see <http://www.gnu.org/licenses/>.
 *
 **/

package fixbugs.core.ir
import scala.util.parsing.combinator.syntactical._
import org.eclipse.jdt.core.dom.InfixExpression.{Operator => Op}
import org.eclipse.jdt.core.dom.PostfixExpression.{Operator => PostOp}

/**
 * Parses Fixbugs transformations
 */
object Parser extends StandardTokenParsers {

  // Common
  //override def skipWhitespace = true

  val operators = List (
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

  val other = List(";",":","(",")","....","=","{","}","::","\"","'","`","%",".") ++
    // side condition
    List("^","|","¬","[","]","@")

  lexical.delimiters ++= (operators ++ postfix ++ other).map(_.toString)

  lexical.reserved += ("if","if2","else","while","try","catch","finally",
    "return","throw","for","do","synchronized","switch","default",
    "case","break","continue","assert","this","super",
    // types
    "int","long","short","byte","float","double","char",
    // Side conditions
    "F","G","X","U","true","True","false","False","is",
    "A","E","and","or","not","mu","nu","type","raw","method",
    "stmt",
    // Transformations and Strategies
    "REPLACE","WITH","WHERE","ADD","METHOD","TO","THEN","DO","PICK","OR","ALWAYS")

  def postFix:Parser[PostOp] = ("++" | "--") ^^ PostOp.toOperator
  def inFix:Parser[Op] = {
    def p(op:Op):Parser[Op] = op.toString ^^ Op.toOperator
    operators.tail.map(p).foldLeft(p(operators.head))(_|_)
  }

  // TODO: arguments for methods, literals
  def arguments = "(" ~> repsep(expression,",") <~ ")"
  def method = (("%" ~> expression) ~ ("." ~> ident) ~ arguments) ^^ {
    case (e~name~args) => Method(e,name,args)
  }
  def namedMethod = (("%" ~> "\"" ~> expression) ~ ("." ~> ident) ~ arguments) ^^ {
    case (e~name~args) => NamedMethod(e,name,args)
  }

  def raw = ("raw" ~> stringLit) ^^ {JavaLiteral(_)}

  def mv = ident ^^ {Metavar(_)}

  def inner:Parser[Expression] = mv | method | namedMethod | raw

  def unary = inner ~ opt(postFix) ^^ (x => x match {
      case e~None => e
      case e~Some(o) => UnOp(e,o)
  })
  
  def expression:Parser[Expression] = (unary ~ opt(inFix ~ expression)) ^^ (x => x match { 
  	case u~None => u
  	case u~Some(op~ex) => BinOp(u,ex,op)
  })

  // definitions of type patterns
  def typedef = metaType | typeLit | primType
  def metaType = "::" ~> ident ^^ {TypeMetavar(_)}
  def typeLit = stringLit ^^ {SimpType(_)}
//"'" ~> (rep1sep(ident,".") ^^ {x => SimpType(x.reduceLeft{(x,acc) => x + "." + acc})}) <~ "'"
  def primType = ("int"|"long"|"short"|"byte"|"float"|"double"|"char") ^^ {PrimType(_)}

  def lb = (ident <~ ":") ~ statement ^^ { case l~s => Label(l,s) }
  def wc = "...." ^^ { _ => Wildcard() }
  def skip = ";" ^^ { _ => Skip() }
  def ass = typedef ~ ident ~ ("=" ~> expression <~ ";") ^^ { case t~v~e => Assignment(t,v,e) }
  def ifelse = ("if"~>"("~>expression<~")")~statement~("else"~>statement) ^^ { case c~t~f => IfElse(c,t,f) }
  def ifelse2 = ("if2"~>"("~>expression<~")")~statement ^^ { case c~t => IfElse(c,t,null) }
  def loop = ("while"~>"("~>expression<~")")~statement ^^ { case c~b => While(c,b) }
  //def tcf = ("try"~>block)~("catch"~>block)~("finally"~>block) ^^ {case t~c~f => TryCatchFinally(t,c,f) }
  def tcf = ("try"~>block)~(catchh*)~opt("finally"~>block) ^^ {
      case t~c~Some(f) => TryCatchFinally(t,c,f)
      case t~c~None => TryCatchFinally(t,c,SBlock(Nil))
  }
  def catchh = (("catch" ~> "(" ~> stringLit) ~ (ident <~ ")") ~ block) ^^ {
    case exp~id~b => CatchClauseStmt(id,exp,b)
  }
  def see = expression <~ ";" ^^ { SideEffectExpr(_) }
  def block = "{" ~> statements <~ "}" ^^ { SBlock(_)}
  def returnStmt = "return" ~> expression <~ ";" ^^ {Return(_)}
  def throww = "throw" ~> expression <~ ":" ^^ {Throw(_)}
  def fors = "for" ~> "(" ~> (forLoop ) //| foreach)
  def forLoop = expression ~ (";" ~> expression) ~ (";" ~> expression <~ ")" ) ~ statement ^^ {
    case init~cond~updaters~stmt => For(List(init),cond,List(updaters),stmt)
  }
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
  def break = "break" ~> ident <~ ";" ^^ { Break(_) }
  def continue = "continue" ~> ident <~ ";" ^^ { Continue(_) }
  def assert = "assert" ~> expression <~ ";" ^^ { Assert(_) }
  def cons = "this" ~> "(" ~> expression <~ ")" ^^ {e => Constructor(List(e))}
  def scons = "super" ~> "(" ~> expression <~ ")" ^^ {e => SuperConstructor(List(e))}

  def recons = "`" ~> ident <~ "`" ^^ { StatementReference(_) }

  def statement:Parser[Statement] =
    lb | wc | ass | ifelse | ifelse2 | loop | tcf | see | block |
    returnStmt | throww | fors | doLoop | sync | switch | default |
    switchcase | break | continue | assert | cons | scons | skip | recons

  def statements:Parser[List[Statement]] = statement*

  // Side Condition Matching
  def future = "F" ~> node ^^ (Future(_))
  def global = "G" ~> node ^^ (Global(_))
  def next = "X" ~> node ^^ (Next(_))
  def until = node ~ "U" ~ node ^^ {case l~_~r => Until(l,r)}
  
  def path = future | global | next | until

  def _true = "true" ^^ (_=>True())
  def _false = "false" ^^ (_=>False())
  def _inner = _true | _false | "(" ~> node <~")" | ident ^^ {NodePred(_)}
  def _unary = _inner |
    ("¬" ~ _inner ^^ {case _~n => Not(n)}) |
    "is"~>ident ^^ (NodePred(_)) |
    "mu"~>ident~node ^^ {case i~n => Mu(i,n)} |
    "nu"~>ident~node ^^ {case i~n => Nu(i,n)} |
    "A"~>"[" ~> path <~"]" ^^ (All(_)) |
    "E"~>"[" ~> path <~ "]" ^^ (Exists(_)) |
    ("stmt" ~> "(" ~> statement <~ ")") ^^ {StmtPred(_)} |
    _inner
  def _and = rep1sep(_unary,"^") ^^ {_.reduceLeft({(x,acc)=>And(x,acc)})}
  def _binary:Parser[NodeCondition] = rep1sep(_and,"|") ^^ {_.reduceLeft({(x,acc)=>Or(x,acc)})}

  /*
  _unary ~ opt(("^" | "|") ~ _binary) ^^ (x => x match {
    case in~None => in
    case l~Some("^"~r) => And(l,r)
    case l~Some("|"~r) => Or(l,r)
  })
  def _binary:Parser[NodeCondition] = _unary ~ opt(("^" | "|") ~ _binary) ^^ (x => x match {
    case in~None => in
    case l~Some("^"~r) => And(l,r)
    case l~Some("|"~r) => Or(l,r)
  })
  */
  def node:Parser[NodeCondition] = _binary
  
  def $inner = "True" ^^ (_=>STrue()) | "False" ^^ (_=>SFalse()) |
    ("type" ~> ident) ~ ("is" ~> typedef) ^^ {case id~typ => TypePred(id,typ)} |
    ("method" ~> stringLit) ^^ {MethodPred(_)}
  def $unary = $inner |
    "(" ~> side <~ ")" |
    ("{"~>node<~"}") ~ "@" ~ ident ^^ {case n~_~v => Satisfies(v,n)} |
    "not" ~> side ^^ {case s => SNot(s)}
  def $binary:Parser[SideCondition] = $unary ~ opt(("and"|"or") ~ $binary) ^^ (x => x match {
    case u~None => u
    case l~Some("and"~r) => SAnd(l,r)
    case l~Some("or"~r) => SOr(l,r)
  })
  
  def side:Parser[SideCondition] = $binary
  
  // Transformations
  def replace = ("REPLACE"~> statement)~ ("WITH" ~> statement) ~ (("WHERE" ~> side)|"ALWAYS") ^^ {
    case from~to~"ALWAYS" => Replace(from,to,STrue())
    case from~to~cond => Replace(from,to,cond.asInstanceOf[SideCondition])
  }

  def vdecl = (typedef ~ mv) ^^ {case (t~i) => VDecl(t,i)}
  def vdecls = rep1sep(vdecl,",")
  def add_method = ("ADD" ~> "METHOD" ~> vdecl) ~ ("(" ~> vdecls <~ ")") ~ statements ~ ("TO" ~> mv) ~ opt("WHERE" ~> side) ^^ {
    case ret~args~stmts~named~cond => AddMethod(ret,args,stmts,named,cond.getOrElse(STrue()))
  }
  def then = "DO" ~> rep1sep(trans,"THEN") ^^ {case l => Then(l)}
  def pick = "PICK" ~> rep1sep(trans,"OR") ^^ {case l => Pick(l)}
  
  def trans:Parser[Transformation] = replace | then | pick
}

