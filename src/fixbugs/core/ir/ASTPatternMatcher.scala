package fixbugs.core.ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.{Map => MMap}
import scala.collection.jcl.MutableIterator.Wrapper
import org.eclipse.jdt.core.dom._
import org.eclipse.jdt.core.dom.{Statement => IRStmt, Expression => IRExpr, Assignment => IRAssign}
import fixbugs.core.ir.{Statement => Stmt,Expression => Expr,Assignment => Assign}

/**
 * Pattern matchs a Fixbugs pattern against the eclipse AST
 * 
 * Proceeds recursively:
 * unify(node,ir,context) returns a true iff the node is unifiable with the ir and updates the environment if so
 * 
 * TODO: blocks, Currently only looking at a /statement level
 */
class ASTPatternMatcher {
   
  var i = 0

  // UTILITY METHODS
  def next = {i+=1;i}
  // context creation
  def c(v:Boolean):Context = c(v,new HashMap())
  def c(v:Boolean,vals:MMap[String,ASTNode]):Context = new Context(v,vals)
  // singleton context
  def one(k:String,v:ASTNode):Context = c(true,HashMap(k->v))
  def guard(g:Boolean,context:()=>Context) = {
    if(g)
      context()
    else
      c(false)
  }
  implicit def javaIteratorToScalaIteratorA[X](it:java.util.Iterator[X]) = new Wrapper[X](it)
  
  def unifyAll(cu:CompilationUnit,pattern:Stmt) = {
    cu.types.iterator.foreach(
      _.asInstanceOf[TypeDeclaration].getMethods.foreach({m =>
        m.getBody.statements.iterator.foreach(
          s => println(unifyStmt(s.asInstanceOf[IRStmt],pattern))
        )
      })
    )
  }
  
  /**
   * DONE:
	   ExpressionStatement
	   VariableDeclarationStatement
       IfStatement
       WhileStatement
       TryStatement
   * TODO:
    Block
    
    ReturnStatement
    ThrowStatement
    ForStatement
    EnhancedForStatement
   
    DoStatement
    SwitchStatement
    SynchronizedStatement
    
    BreakStatement
    ContinueStatement
    AssertStatement
    TypeDeclarationStatement
    ConstructorInvocation
    SuperConstructorInvocation
   * Ignore:
    EmptyStatement
    LabeledStatement
   */
  def unifyStmt(node:IRStmt,pattern:Stmt):Context =  {
    (node,pattern) match {
      
      case (stmt,Label(lbl,statement)) => unifyStmt(stmt,statement) & one(lbl,stmt)
      case (stmt,Wildcard()) => c(true)
      
      case (stmt:VariableDeclarationStatement,Assign(name,to)) => {
        // TODO: multiple declarations
        val frag = stmt.fragments.get(0).asInstanceOf[VariableDeclarationFragment]
        unifyExpr(frag.getInitializer,to) & one(name,frag.getName)
      }
      case (stmt:IfStatement,IfElse(cond,tb,fb)) =>
        unifyExpr(stmt.getExpression,cond) & unifyStmt(stmt.getThenStatement(),tb) & unifyStmt(stmt.getElseStatement(),fb)
      case (stmt:WhileStatement,While(cond,body)) =>
        unifyExpr(stmt.getExpression,cond) & unifyStmt(stmt.getBody,body)
      case (stmt:TryStatement,TryCatchFinally(tryB,catchB,finallyB)) => {
        // TODO: multiple catch clauses
        val cath = stmt.catchClauses.get(0).asInstanceOf[CatchClause]
        unifyStmt(stmt.getBody,tryB) & unifyStmt(cath.getBody,catchB) & unifyStmt(stmt.getFinally,finallyB)
      }
      
      // Error cases
      case (_:Block,_) => throw new Exception("currently not dealing with blocks")
      case (_,_:SBlock) => throw new Exception("currently not dealing with blocks")
      case (_:CompilationUnit,_) => throw new Exception("Compilation Units shouldn't be here")
      case _ => c(false)
    }
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
   * TODO:
    ClassInstanceCreation
    ArrayCreation
    ArrayInitializer
    VariableDeclarationExpression
    CastExpression
    InstanceofExpression
    ConditionalExpression
    Assignment
    SuperMethodInvocation
   * 
   *  NB: arguments, it ignores longer pairs
   */
  def unifyExpr(node:IRExpr,pattern:Expr):Context = {
    (node,pattern) match {
      case (expr,Metavar(name)) => one(name,expr)
      case (expr:MethodInvocation,Method(name,args)) => {
        // TODO: expression before method call matching
        val margs = expr.arguments.asInstanceOf[java.util.List[IRExpr]].iterator.zip(args.elements)
        one(name,expr.getName) & margs.map({case (e,p) => unifyExpr(e,p)}).foldLeft(c(true))(_&_)
      }
      case (expr:InfixExpression,BinOp(left,right,op)) =>
        guard(op == expr.getOperator,()=>unifyExpr(expr.getLeftOperand,left) & unifyExpr(expr.getRightOperand,right))
      case (expr:PostfixExpression,UnOp(inner,op)) =>
        guard(op == expr.getOperator,()=>unifyExpr(expr.getOperand,inner))
      case (expr:PrefixExpression,UnOp(inner,op)) =>
        guard(op == expr.getOperator,()=>unifyExpr(expr.getOperand,inner))
      case (expr:ParenthesizedExpression,pattern) => unifyExpr(expr.getExpression,pattern)
      case _ => c(false)
    }
  }
}

/**
 * Immutable Context Object
 * Note: wraps mutable map for simplicity
 */
class Context(st:Boolean,vals:MMap[String,ASTNode]) extends Cloneable[Context] {
  
  val values = vals
  val status = st
  
  /**
   * NB: lazily creates other context
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
    if (status) {
      val k = (Set() ++ values.keys) ** (Set() ++ other.values.keys)
      status &= k.map(k => values(k) == other.values(k)).foldLeft(true)(_&&_)
    }
    new Context(status,values ++ other.values)
  }
  
  def add(arg:(String,ASTNode)):Context = {
    val (metavar,node) = arg
    val old = values(metavar)
    if(old != node)
      new Context(false,values)
    else
      new Context(true,values + (metavar -> node))
  }
  
}
