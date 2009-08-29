package fixbugs.core.ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.{Map => MMap}
import scala.collection.jcl.MutableIterator.Wrapper
import org.eclipse.jdt.core.dom._
import org.eclipse.jdt.core.dom.rewrite._
import org.eclipse.jdt.core.dom.{Statement => IRStmt, Expression => IRExpr, Assignment => IRAssign}
import fixbugs.core.ir.{Statement => Stmt,Expression => Expr,Assignment => Assign}
import fixbugs.util.EclipseUtil.parse
import scala.collection.mutable.{Map => MMap}

/**
 * Generates Eclipse AST for replacement, from a fixbugs Pattern and a Context
 * 
 */
class ASTPatternGenerator(ast:AST,rewrite:ASTRewrite, context:Map[String,ASTNode]) {
  
  val con = MMap() ++ context.map({case (k,v) => (k,(v,false))})

  def get[X](name:String):X = {
      val (x,used) = con(name)
      if(! used) {
        con += name -> (x,true)
        x.asInstanceOf[X]
      } else {
        ASTNode.copySubtree(ast,x).asInstanceOf[X]
      }
  }

  /**
   * Generates Statements
   */
  def generate(stmt:Stmt):IRStmt = stmt match {
    case Wildcard() => throw new Exception("Internal Error, cannot generate anything for a wildcard")
    case Assignment(typee,what,init) => {
        val assign = ast.newVariableDeclarationFragment
        assign.setInitializer(generate(init))
        assign.setName(get(what))
        val assignStmt = ast.newVariableDeclarationStatement(assign)
        assignStmt.setType(generate(typee))
        assignStmt
    }
    case IfElse(cond,thenBlock,elseBlock) => {
        val stmt = ast.newIfStatement
        stmt.setExpression(generate(cond))
        stmt.setThenStatement(generate(thenBlock))
        stmt.setElseStatement(generate(elseBlock))
        stmt
    }
    case Assert(cond) => {
        val stmt = ast.newAssertStatement
        stmt.setExpression(generate(cond))
        stmt
    }
    case Throw(expr) => {
        val stmt = ast.newThrowStatement
        stmt.setExpression(generate(expr))
        stmt
    }
    case SideEffectExpr(expr) => ast.newExpressionStatement(generate(expr))
    case Return(expr) => {
        val stmt = ast.newReturnStatement
        stmt.setExpression(generate(expr))
        stmt
    }
    case Continue(lbl) => {
        val stmt = ast.newContinueStatement
        stmt.setLabel(get(lbl))
        stmt
    }
    case Break(lbl) => {
        val stmt = ast.newBreakStatement
        stmt.setLabel(get(lbl))
        stmt
    }
    case Do(body,cond) => {
        val stmt = ast.newDoStatement
        stmt.setBody(generate(body))
        stmt.setExpression(generate(cond))
        stmt
    }
    case While(cond,body) => {
        val stmt = ast.newWhileStatement
        stmt.setBody(generate(body))
        stmt.setExpression(generate(cond))
        stmt
    }
    case Synchronized(body,lock) => {
        val stmt = ast.newSynchronizedStatement
        stmt.setBody(generateBlock(body.asInstanceOf[SBlock]))
        stmt.setExpression(generate(lock))
        stmt
    }
    case Constructor(args) => {
        val stmt = ast.newConstructorInvocation
        addArgs(stmt,args)
        stmt
    }
    case SuperConstructor(args) => {
        val stmt = ast.newSuperConstructorInvocation
        addArgs(stmt,args)
        stmt
    }
    case Switch(stmts,expr) => {
        val stmt = ast.newSwitchStatement
        addToStmt(stmt,stmts)
        stmt.setExpression(generate(expr))
        stmt
    }
    case Switchcase(expr) => {
        val stmt = ast.newSwitchCase
        stmt.setExpression(generate(expr))
        stmt
    }
    case DefaultCase() => {
        val default = ast.newSwitchCase
        // This makes me cry a little
        default.setExpression(null)
        default
    }
    case Label(_,_) => throw new Exception("Label Patterns aren't reconstructable")
    case TryCatchFinally(tryB,catchB,finallyB) => {
        val stmt = ast.newTryStatement
        stmt.setBody(generateBlock(tryB))
        // TODO: catch block
        val catches = stmt.catchClauses.asInstanceOf[java.util.List[CatchClause]]
        stmt.setFinally(generateBlock(finallyB))
        stmt
    }
    case SBlock(stmts) => generateBlock(SBlock(stmts))
    case ForEach(typee,id,expr,body) => {
        val decl = ast.newSingleVariableDeclaration
        decl.setType(generate(typee))
        decl.setName(get(id))
        val stmt = ast.newEnhancedForStatement
        stmt.setExpression(generate(expr))
        stmt.setBody(generate(body))
        stmt.setParameter(decl)
        stmt
    }
    case For(init,cond,updaters,body) => {
        val stmt = ast.newForStatement
        stmt.setExpression(generate(cond))
        stmt.setBody(generate(body))
        addExprs(stmt.initializers,init)
        addExprs(stmt.updaters,updaters)
        stmt
    }
  }

  def generateBlock(sblock:SBlock):Block = {
    val block = ast.newBlock
    addToStmt(block,sblock.stmts)
    block
  }

  def addArgs(stmt:{ def arguments():java.util.List[_]},args:List[Expr]) = addExprs(stmt.arguments,args)
  
  def addExprs(exprs:java.util.List[_],vals:List[Expr]) = {
    val argList = exprs.asInstanceOf[java.util.List[IRExpr]]
    for(arg <- vals)
        argList.add(generate(arg))
  }
  
  def addToStmt(stmt:{ def statements():java.util.List[_]},vals:List[Stmt]) = addStmts(stmt.statements,vals)
  def addStmts(exprs:java.util.List[_],vals:List[Stmt]) = {
    val argList = exprs.asInstanceOf[java.util.List[IRStmt]]
    for(arg <- vals)
        argList.add(generate(arg))
  }

  /**
   * Generates expressions
   * Java/Eclipse Idioms make this so sigh
   */
  def generate(expr:Expression):IRExpr = expr match {
    case Metavar(name) => get(name)
    case Method(name,args) => get(name)
    case BinOp(l,r,op) => {
        val expr = ast.newInfixExpression
        expr.setLeftOperand(generate(l))
        expr.setRightOperand(generate(r))
        expr.setOperator(op)
        expr
    }
    case UnOp(inner,op) => {
        val expr = ast.newPostfixExpression
        expr.setOperand(generate(inner))
        expr.setOperator(op)
        expr
    }
    case Cast(inner,typee) => {
        val expr = ast.newCastExpression
        expr.setExpression(generate(inner))
        expr.setType(generate(typee))
        expr
    }
    case New(typee,args) => {
        val expr = ast.newClassInstanceCreation
        expr.setType(generate(typee))
        val argList = expr.arguments.asInstanceOf[java.util.List[IRExpr]]
        for(arg <- args)
            argList.add(generate(arg))
        expr
    }
    case InstanceOf(typee,inner) => {
        val expr = ast.newInstanceofExpression
        expr.setLeftOperand(generate(inner))
        expr.setRightOperand(generate(typee))
        expr
    }
    case ArrayInit(exprs) => {
        val expr = ast.newArrayInitializer
        addExprs(expr.expressions,exprs)
        expr
    }
  }

  def generate(typee:TypePattern):Type = typee match {
    case PrimType(name) => ast.newPrimitiveType(PrimitiveType.toCode(name))
    case SimpType(name) => ast.newSimpleType(ast.newName(name))
    case ArraType(ofType) => ast.newArrayType(generate(ofType))
    case TypeMetavar(name) => get(name)
  }

}
