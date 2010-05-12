
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

import scala.collection.mutable.{HashMap,Set => MSet}
import scala.collection.mutable.{Map => MMap}
import scala.collection.jcl.MutableIterator.Wrapper
import org.eclipse.jdt.core.dom._
import org.eclipse.jdt.core.dom.{Statement => IRStmt, Expression => IRExpr, Assignment => IRAssign}
import fixbugs.core.ir.{Statement => Stmt,Expression => Expr,Assignment => Assign}
import fixbugs.util.EclipseUtil.parse

import org.slf4j.{Logger,LoggerFactory}

/**
 * Pattern matchs a Fixbugs pattern against the eclipse AST
 * 
 * Proceeds recursively:
 * unify(node,ir,context) returns a true iff the node is unifiable with the ir and updates the environment if so
 * 
 */
class ASTPatternMatcher {

  val log = LoggerFactory getLogger(this getClass)

  var i = 0

  // UTILITY METHODS
  def next = {i+=1;i}
  // context creation
  def c(v:Boolean):Context = c(v,new HashMap())
  def c(v:Boolean,vals:MMap[String,ASTNode]):Context = new Context(v,vals,MSet())
  // singleton context
  def one(k:String,v:ASTNode):Context = c(true,HashMap(k->v))
  def guard(g:Boolean,context:()=>Context) = {
    if(g)
      context()
    else
      c(false)
  }
  implicit def javaItToScalaIt[X](it:java.util.Iterator[X]) = new Wrapper[X](it)
  //implicit def javaItToScalaList[X](it:java.util.Iterator[X]):List[X] = List() ++ it
  implicit def compress[X](ar:Array[X]):Iterator[X] = ar.elements
 
  // should only be called in testing
  protected def unifyAll(src:String,pattern:Stmt):Iterator[Context] = unifyAll(parse(src),pattern)

  var debugIndent = 0
  def tabs() = (0 to debugIndent).foldLeft(new StringBuilder){(acc,_) => acc.append("\t")} toString

  var wildcards:List[List[IRStmt]] = Nil

  /**
   * Accumulate all contexts for a compilation unit
   * TODO: initializers, constructors, fields
   */
  def unifyAll(cu:CompilationUnit,pattern:Stmt):Iterator[Context] = {
    wildcards = Nil
    val acc = cu.types.iterator.flatMap(
      _.asInstanceOf[TypeDeclaration].getMethods.flatMap({m =>
        val inner = m.getBody.statements
        pattern match {
            case SBlock(patterns) => List(blockIt(inner,patterns))
            case _ => inner.iterator.map(s => unifyStmt(s.asInstanceOf[IRStmt],pattern)).toList.filter(_.status)
        }
      })
    )
    acc
  }
  
    // Sigh collection conversions
    def blockIt(stmts:java.util.List[_],pattern:List[Statement]):Context = {
      val it = stmts.iterator
      var l = List[IRStmt]()
      while(it.hasNext)
        l += it.next.asInstanceOf[IRStmt]
      block(l,pattern,Nil)
    }
    
	def block(stmts:List[IRStmt],pattern:List[Statement],wildcard:List[IRStmt]):Context = {
	  val res = ((stmts,pattern) match {
	    case (s::ss,Wildcard()::p::ps) => {
          log debug("Block - Wildcard w Stmt: %s (%d) %s (%d)".format(s.getClass.getName,stmts.size,p,pattern.size))
	      val attempt = unifyStmt(s,p)
          log debug ("Attempt.status: {},wildcard = {}",attempt.status,wildcard)
          if (attempt.status) {
            wildcards = wildcard :: wildcards
            log debug ("wildcards now {}", wildcards)
            attempt && (()=> block(ss,ps,Nil))
          } else {
            block(ss,Wildcard()::p::ps,s::wildcard)
          }
        }
	    case (s::ss,p::ps) => {
          log debug("Block - Stmt w Stmt: %s (%d) %s (%d)".format(s.getClass.getName,stmts.size,p,pattern.size))
          unifyStmt(s,p) && (()=>block(ss,ps,Nil))
        }
	    case (Nil,Wildcard() :: Nil) => {
            wildcards = wildcard :: wildcards
            c(true)
        }
	    case (Nil,Nil) => {
            c(true)
        }
	    case _ => {
            log debug("Hit Base match in block")
            c(false)
        }
	  })
      log debug ("Block Res: %s for %d and %d".format(res.status,stmts.size,pattern.size))
      res
	}
  
  /**
   * Big TODO: product with recursive calls, anonymous inner classes
   * DONE:
       ExpressionStatement
       VariableDeclarationStatement
       IfStatement
       WhileStatement
       TryStatement
       LabeledStatement
       ReturnStatement
       ThrowStatement
       ForStatement
       EnhancedForStatement
       Block
       DoStatement
       SynchronizedStatement
       SwitchStatement
       BreakStatement
       ContinueStatement
       AssertStatement
       ConstructorInvocation
       SuperConstructorInvocation
   * TODO:
    TypeDeclarationStatement
   * Ignore:
    EmptyStatement
   */
  def unifyStmt(node:IRStmt,pattern:Stmt):Context = {
    log debug("Statement: "+node.getClass.getName+" {}",pattern)
    val con = (node,pattern) match {
     
      case (stmt,Label(lbl,statement)) => {
          log debug ("Label: {}",unifyStmt(stmt,statement).status)
          unifyStmt(stmt,statement) && (() => one(lbl,stmt))
      }
      case (stmt,Wildcard()) => c(true)
      case (stmt:EmptyStatement,Skip()) => c(true)
      // fuzzy matching
      // wildcard matches many statements
      case (stmt:Block,SBlock(stmts)) => {
    	  // recurse over list, destroying
          blockIt(stmt.statements,stmts)
      }

      // block matching for single statement blocks
      case (stmt:Block,pat) => {
    	  val stmts = stmt.statements
    	  guard (stmts.size == 1, () => unifyStmt(stmts.get(0).asInstanceOf[IRStmt],pat))
      }
      
      case (stmt:ExpressionStatement,SideEffectExpr(expr)) => unifyExpr(stmt.getExpression,expr)
      case (stmt:VariableDeclarationStatement,Assign(typee,name,to)) => {
        // TODO: multiple declarations
        val frag = stmt.fragments.get(0).asInstanceOf[VariableDeclarationFragment]
        checkType(stmt.getType,typee) && (() => unifyExpr(frag.getInitializer,to) & one(name,frag.getName))
      }
      case (stmt:IfStatement,IfElse(cond,tb,fb)) => {
        val always = unifyExpr(stmt.getExpression,cond) & unifyStmt(stmt.getThenStatement(),tb)
        val pat = fb == null
        val elseStmt = stmt.getElseStatement == null
        if (pat && elseStmt)
            always
        else if (!pat && !elseStmt)
            always & unifyStmt(stmt.getElseStatement(),fb)
        else
            c(false)
      }
      case (stmt:WhileStatement,While(cond,body)) =>
        unifyExpr(stmt.getExpression,cond) & unifyStmt(stmt.getBody,body)
      case (stmt:DoStatement,Do(body,cond)) =>
        unifyExpr(stmt.getExpression,cond) & unifyStmt(stmt.getBody,body)
      case (stmt:TryStatement,TryCatchFinally(tryB,catchB,finallyB)) => {
        var always = unifyStmt(stmt.getBody,tryB) & unifyStmt(stmt.getFinally,finallyB)
        if(stmt.catchClauses.size != catchB.size)
            c(false)
        else if (catchB.isEmpty)
            always
        else {
            val javaCatch = stmt.catchClauses.get(0).asInstanceOf[CatchClause]
            val CatchClauseStmt(name,typee,block) = catchB.head
            val exp = javaCatch.getException
            always &= unifyStmt(javaCatch.getBody,block) & one(name,exp.getName)
            always & c(exp.getType.toString.equals(typee))
            // TODO: multiple catch clauses
        }
      }
      case (stmt:LabeledStatement,pat) => unifyStmt(stmt.getBody,pat)
      case (stmt:ReturnStatement,Return(expr)) => unifyExpr(stmt.getExpression,expr)
      case (stmt:ThrowStatement,Throw(expr)) => unifyExpr(stmt.getExpression,expr)
      case (stmt:ForStatement,For(init,cond,up,body)) =>
        unifyExprs(stmt.initializers,init) & unifyExpr(stmt.getExpression,cond) &
        unifyExprs(stmt.updaters,up) & unifyStmt(stmt.getBody,body)
      case (stmt:EnhancedForStatement,ForEach(typee,id,expr,body)) => {
        val param = stmt.getParameter
        checkType(param.getType,typee) & 
        one(id,param.getName) &
        unifyExpr(stmt.getExpression,expr) &
        unifyStmt(stmt.getBody,body)
      }
      case (stmt:SynchronizedStatement, Synchronized(body,lock)) =>
      	unifyExpr(stmt.getExpression,lock) && (() => unifyStmt(stmt.getBody,body))
      case (stmt:SwitchStatement, Switch(stmts,cond)) =>
		unifyExpr(stmt.getExpression,cond) && (() => blockIt(stmt.statements,stmts))
      case (stmt:SwitchCase,DefaultCase()) => c(stmt.isDefault)
      case (stmt:SwitchCase,Switchcase(expr)) => unifyExpr(stmt.getExpression,expr)
      case (stmt:BreakStatement,Break(mv)) => one(mv,stmt.getLabel)
      case (stmt:ContinueStatement,Continue(mv)) => one(mv,stmt.getLabel)
      case (stmt:AssertStatement,Assert(expr)) => unifyExpr(stmt.getExpression,expr)
      case (stmt:ConstructorInvocation,Constructor(exprs)) =>
      	unifyExprs(stmt.arguments,exprs)
      case (stmt:SuperConstructorInvocation,SuperConstructor(exprs)) =>
      	unifyExprs(stmt.arguments,exprs)
      case _ => c(false)
    }
    con.values += "_from" -> node
    con.replaceNodes += node
    log.debug ("con.status = {}",con.status)
    con
  }

  /**
   * 
   * Only matching Metavar:
    Name
    IntegerLiteral (includes decimal, hex, and octal forms; and long)
    FloatingPointLiteral (includes both float and double)
    CharacterLiteral
    NullLiteral
    BooleanLiteral
    StringLiteral
    TypeLiteral
    ThisExpression
    SuperFieldAccess
    FieldAccess
    ArrayAccess
   * Matched:
    MethodInvocation
    ParenthesizedExpression
    InfixExpression
    PostfixExpression
    PrefixExpression
    CastExpression
    ClassInstanceCreation
    InstanceofExpression
    ArrayInitializer
   * TODO:
    ArrayCreation
    VariableDeclarationExpression
    ConditionalExpression
    Assignment
    SuperMethodInvocation
   * 
   *  NB: arguments, it ignores longer pairs
   */
  def unifyExpr(node:IRExpr,pattern:Expr):Context = {
    log debug(tabs()+"Expression: "+node.getClass.getName+" {}",pattern)
    debugIndent += 1
    val res = (node,pattern) match {
      //case (_,Anything()) => c(true)
      case (expr,Metavar(name)) => one(name,expr)
      case (expr:MethodInvocation,Method(callOn,name,args)) => {
        log debug ("e: {}",unifyExpr(expr.getExpression,callOn).status)
        log debug ("args: {}",unifyExprs(expr.arguments,args).status)
        log debug ("one: {}",one(name,expr.getName).status)
        log debug ("all: {}",(unifyExpr(expr.getExpression,callOn) & unifyExprs(expr.arguments,args) & one(name,expr.getName)).status)
        unifyExpr(expr.getExpression,callOn) & unifyExprs(expr.arguments,args) & one(name,expr.getName)
      }
      case (expr:MethodInvocation,NamedMethod(callOn,name,args)) => {
        log debug ("c: %s %s %s ".format(name.equals(expr.getName.toString),name,expr.getName))
        unifyExpr(expr.getExpression,callOn) & unifyExprs(expr.arguments,args) & c(name.equals(expr.getName.toString))
      }
      case (expr:InfixExpression,BinOp(left,right,op)) =>
        guard(op == expr.getOperator,()=>unifyExpr(expr.getLeftOperand,left) & unifyExpr(expr.getRightOperand,right))
      case (expr:PostfixExpression,UnOp(inner,op)) =>
        guard(op == expr.getOperator,()=>unifyExpr(expr.getOperand,inner))
      case (expr:PrefixExpression,UnOp(inner,op)) =>
        guard(op == expr.getOperator,()=>unifyExpr(expr.getOperand,inner))
      case (expr:ParenthesizedExpression,pattern) => unifyExpr(expr.getExpression,pattern)
      case (expr:CastExpression,Cast(inner,typee)) =>
        checkType(expr.getType,typee) && (()=>unifyExpr(expr.getExpression,inner))
      case (expr:ClassInstanceCreation,New(typee,args)) =>
        checkType(expr.getType,typee) && (()=>unifyExprs(expr.arguments,args))
      case (expr:InstanceofExpression,InstanceOf(typee,inner)) =>
        checkType(expr.getRightOperand,typee) && (()=>unifyExpr(expr.getLeftOperand,inner))
      case (expr:ArrayInitializer,ArrayInit(exprs)) => unifyExprs(expr.expressions,exprs)
      case (expr:BooleanLiteral,JavaLiteral(value)) => c(value.toBoolean == expr.booleanValue)
      case (expr:NumberLiteral,JavaLiteral(value)) => c(value.equals(expr.getToken))
      case (expr:NullLiteral,JavaLiteral(value)) => c(value.equals("null"))
        
      case _ => c(false)
    }
    debugIndent -= 1
    log debug(tabs()+"Res: {}",res.status)
    res
  }
  
  def unifyExprs(arguments:java.util.List[_],args:List[Expression]) = {
    log debug(tabs()+"ExpressionS: "+arguments.size+" {}",args.size)
    debugIndent += 1
    val margs = arguments.asInstanceOf[java.util.List[IRExpr]].iterator.zip(args.elements)
    val res = margs.map({case (e,p) => unifyExpr(e,p)}).foldLeft(c(true))(_&_)
    debugIndent -= 1
    log debug(tabs()+"Res: {}",res.status)
    res
  }
  
  /**
   * TODO: Parameterized Types, Wildcard Types, Qualified Type
   * @see http://help.eclipse.org/galileo/index.jsp?topic=/org.eclipse.jdt.doc.isv/reference/api/org/eclipse/jdt/core/dom/CastExpression.html
   */
  def checkType(node:Type,pattern:TypePattern):Context = {
    (node,pattern) match {
      case (t:PrimitiveType,PrimType(name)) => c(name == t.getPrimitiveTypeCode.toString)
      case (t:SimpleType,SimpType(name)) => c(name == t.getName)
      case (t:ArrayType,ArraType(typee)) => checkType(t.getComponentType,typee)
      case (t,TypeMetavar(name)) => one(name,t)
      case _ => c(false)
    }
  }
  
}

/**
 * Immutable Context Object
 * Note: wraps mutable map for simplicity
 */
class Context(st:Boolean,vals:MMap[String,ASTNode],replace:MSet[ASTNode]) extends Cloneable[Context] with Function1[String, ASTNode] {
  
  val log = LoggerFactory getLogger(this getClass)
  
  val values = vals
  val status = st
  val replaceNodes = replace
  
  def apply(name:String) = values(name)

  /**
   * NB: lazily creates other context
   * TODO: figure out if this makes complete sense - should it be set based?
   */
  def ||(other:(()=>Context)):Context = {
    if(status)
      clone.asInstanceOf[Context]
    else
      other()
  }
  
  /**
   * Requires both status to be true
   */
  def &(other:Context):Context = {
    var status = this.status && other.status
    log debug ("& comparison: %s, %s, %s".format(status,values,other.values))
    if (status) {
      val k = ((Set() ++ values.keys) ** (Set() ++ other.values.keys)) - "_from"
      status &= k.map(k => {
          val eq = values(k).toString.equals(other.values(k).toString)
          if (!eq) log debug ("& fail on key %s for %s and %s".format(k,values(k),other.values(k)))
          eq
      }).foldLeft(true)(_&&_)
    }
    new Context(status,values ++ other.values,replaceNodes ++ other.replaceNodes)
  }

  def &&(other:()=>Context):Context = if(this.status) this & other() else this
  
  def add(arg:(String,ASTNode)):Context = {
    val (metavar,node) = arg
    val old = values(metavar)
    if(old != node)
      new Context(false,values,replaceNodes)
    else
      new Context(true,values + (metavar -> node),replaceNodes)
  }
  
  override def toString = {
    "C(status = {%s}, values = {%s})".format(status,values) 
  }
  
}
