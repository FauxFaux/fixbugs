package fixbugs.core.ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.{Map => MMap}
import scala.collection.jcl.MutableIterator.Wrapper
import org.eclipse.jdt.core.dom._
import org.eclipse.jdt.core.dom.{Statement => IRStmt, Expression => IRExpr, Assignment => IRAssign}
import fixbugs.core.ir.{Statement => Stmt,Expression => Expr,Assignment => Assign}
import fixbugs.util.EclipseUtil.parse

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
  implicit def javaItToScalaIt[X](it:java.util.Iterator[X]) = new Wrapper[X](it)
  implicit def javaItToScalaList[X](it:java.util.Iterator[X]):List[X] = List() ++ new Wrapper[X](it)
  implicit def compress[X](ar:Array[X]):Iterator[X] = ar.elements
  
  def unifyAll(src:String,pattern:Stmt):Iterator[Context] = unifyAll(parse(src),pattern)
  
  /**
   * Accumulate all contexts for a compilation unit
   * TODO: initializers, constructors, fields
   */
  def unifyAll(cu:CompilationUnit,pattern:Stmt):Iterator[Context] = {
    cu.types.iterator.flatMap(
      _.asInstanceOf[TypeDeclaration].getMethods.flatMap({m =>
        m.getBody.statements.iterator.map(
          s => unifyStmt(s.asInstanceOf[IRStmt],pattern)
        ).toList.filter(_.status)
      })
    )
  }
  
  /**
   * Big TODO: convert to blocks, and product with recursive calls, anonymous inner classes
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
   * TODO:
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
   */
  def unifyStmt(node:IRStmt,pattern:Stmt):Context = {
    //println(node.getClass)
    (node,pattern) match {
     
      case (stmt,Label(lbl,statement)) => unifyStmt(stmt,statement) & one(lbl,stmt)
      case (stmt,Wildcard()) => c(true)
      
      // fuzzy matching
      // wildcard matches many statements
      // TODO: ignore non-matching prefix/suffix
      case (stmt:Block,SBlock(stmts)) => {
	// recurse over list, destroying
	def block(stmts:List[IRStmt],pattern:List[Statement]) = {
	  (stmts,pattern) match {
	    case (s::ss,Wildcard()::p::ps) =>
	      unifyStmt(s,p) && ()=> block(ss,ps) || block(ss,WildCard()::p::ps)
	    case (s::ss,p::ps) => unifyStmt(s,p) && ()=>block(ss,ps)
	    case (Nil,Wildcard()) => c(true)
	    case (Nil,_) => c(false)
	  }
	}
	block(stmt.getStatements.iterator,stmts)
      }

      // block matching for single statement blocks
      case (stmt:Block,pat) => {
	stmts = stmt.getStatements
	guard (stmts.size == 1, () => unifyStmt(stmt.get(0),pat))
      }
      
      case (stmt:ExpressionStatement,SideEffectExpr(expr)) => unifyExpr(stmt.getExpression,expr)
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
      case (stmt:LabeledStatement,pat) => unifyStmt(stmt.getBody,pat)
      case (stmt:ReturnStatement,Return(expr)) => unifyExpr(stmt.getExpression,expr)
      case (stmt:ThrowStatement,Throw(expr)) => unifyExpr(stmt.getExpression,expr)
      case (stmt:ForStatement,For(init,cond,up,body)) =>
        unifyExprs(stmt.initializers,init) & unifyExpr(stmt.getExpression,cond) &
        unifyExprs(stmt.updaters,up) & unifyStmt(stmt.getBody,body)
      case (stmt:EnhancedForStatement,ForEach(typee,id,expr,body)) => {
        val param = stmt.getParameter
        guard(checkType(param.getType,typee),() =>
          one(id,param.getName) &
          unifyExpr(stmt.getExpression,expr) &
          unifyStmt(stmt.getBody,body)
        )
      }
      
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
    (node,pattern) match {
      case (expr,Metavar(name)) => one(name,expr)
      // TODO: expression before method call matching
      case (expr:MethodInvocation,Method(name,args)) =>
        unifyExprs(expr.arguments,args) & one(name,expr.getName)
      case (expr:InfixExpression,BinOp(left,right,op)) =>
        guard(op == expr.getOperator,()=>unifyExpr(expr.getLeftOperand,left) & unifyExpr(expr.getRightOperand,right))
      case (expr:PostfixExpression,UnOp(inner,op)) =>
        guard(op == expr.getOperator,()=>unifyExpr(expr.getOperand,inner))
      case (expr:PrefixExpression,UnOp(inner,op)) =>
        guard(op == expr.getOperator,()=>unifyExpr(expr.getOperand,inner))
      case (expr:ParenthesizedExpression,pattern) => unifyExpr(expr.getExpression,pattern)
      case (expr:CastExpression,Cast(inner,typee)) =>
        guard(checkType(expr.getType,typee),()=>unifyExpr(expr.getExpression,inner))
      case (expr:ClassInstanceCreation,New(typee,args)) =>
        guard(checkType(expr.getType,typee),()=>unifyExprs(expr.arguments,args))
      case (expr:InstanceofExpression,InstanceOf(typee,inner)) =>
        guard(checkType(expr.getRightOperand,typee),()=>unifyExpr(expr.getLeftOperand,inner))
      case (expr:ArrayInitializer,ArrayInit(exprs)) => unifyExprs(expr.expressions,exprs)
        
      case _ => c(false)
    }
  }
  
  def unifyExprs(arguments:java.util.List[_],args:List[Expression]) = {
    val margs = arguments.asInstanceOf[java.util.List[IRExpr]].iterator.zip(args.elements)
    margs.map({case (e,p) => unifyExpr(e,p)}).foldLeft(c(true))(_&_)
  }
  
  /**
   * TODO: Parameterized Types, Wildcard Types, Qualified Type
   * @see http://help.eclipse.org/galileo/index.jsp?topic=/org.eclipse.jdt.doc.isv/reference/api/org/eclipse/jdt/core/dom/CastExpression.html
   */
  def checkType(node:Type,pattern:TypePattern):Boolean = {
    (node,pattern) match {
      case (t:PrimitiveType,PrimType(name)) => name == t.getPrimitiveTypeCode.toString
      case (t:SimpleType,SimpType(name)) => name == t.getName
      case (t:ArrayType,ArraType(typee)) => checkType(t.getComponentType,typee) 
      case _ => false
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

  def &&(other:()=>Context):Context = &(other())
  
  def add(arg:(String,ASTNode)):Context = {
    val (metavar,node) = arg
    val old = values(metavar)
    if(old != node)
      new Context(false,values)
    else
      new Context(true,values + (metavar -> node))
  }
  
  override def toString = {
    "C(status = {%s}, values = {%s})".format(status,values) 
  }
  
}
