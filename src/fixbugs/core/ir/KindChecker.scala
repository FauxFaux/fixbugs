
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
import scala.collection.mutable.{Map => MMap,HashMap}

import scala.util.matching.Regex

/**
 * Simple properties are checked:
 * A metavariable must be either Expression or Statement or Type kind
 * Metavariables used in generation must be bound
 * Restriction on metavariable naming
 * 
 * Approach: generate local constraints and look for conflicts
 */
object KindChecker {

    val banned = List("_from")

    val StmtMatch = "(stmt\\d+)".r

    type env = MMap[String,Kind]
    def Env():env = 
        new HashMap[String,Kind]() {
            // only use this form of ++
            override def ++ (kvs : Iterable[(String, Kind)]) : env  = {
                for((k,v) <- kvs) {
                    if(this.getOrElse(k,v) != v)
                        throw new KindCheckingException("Type error: "+k+" can be either "+v+" or "+this(k))
                    else
                        this += (k -> v)
                }
                this
            }
        }
    def Env(args:(String,Kind)):env = Env() + args

    def check(t:Transformation):Unit = t match {
        case Replace(from,to,cond) => {
            val (fromEnv,toEnv) = (aggStmt(from),aggStmt(to))
            
            val names = Set() ++ toEnv.keys ++ fromEnv.keys
            if (names contains "_from")
                throw new KindCheckingException("You may not use '_from' as a variable name")
            for(n <- names) {
                n match {
                    case StmtMatch(x) => throw new KindCheckingException(x + "is an invalid variable name")
                    case _ => ()
                }
            }

            val unbound = (Set() ++ toEnv.keys) -- (Set() ++ fromEnv.keys)
            if (! unbound.isEmpty)
                throw new KindCheckingException("The following variables are unbound (ie used in replacement, but not creation: "+unbound)
            ()
        }
        case Then(transformations) => {
            for(t <- transformations)
                check(t)
        }
        case Pick(transformations) => {
            for(t <- transformations)
                check(t)
        }
    }

    def aggStmts(stmts:List[Statement]):env = stmts.map(aggStmt).foldLeft(Env()){_ ++ _}

    def aggStmt(stmt:Statement):env = stmt match {
        case Label(name,inner) => Env(name -> StatementKind()) ++ aggStmt(inner)
        case Assignment(typee,name,expr) => Env(name -> StatementKind()) ++ aggType(typee) ++ aggExpr(expr)
        case IfElse(cond,then,otherwise) => aggExpr(cond) ++ aggStmt(then) ++ aggStmt(otherwise)
        case While(cond,body) => aggExpr(cond) ++ aggStmt(body)
        case TryCatchFinally(tri,catc,finall) => catc
            .map({case CatchClauseStmt(_,_,stmts) => aggStmt(stmts)})
            .foldLeft(aggStmt(tri) ++ aggStmt(finall)) {_ ++ _}
        case SideEffectExpr(expr) => aggExpr(expr)
        case SBlock(stmts) => aggStmts(stmts)
        case Return(expr) => aggExpr(expr)
        case Throw(expr) => aggExpr(expr)
        case For(inits,cond,updaters,body) => aggExprs(inits) ++ aggExpr(cond) ++ aggExprs(updaters) ++ aggStmt(body)
        case ForEach(typee,id,expr,body) => aggType(typee) ++ Env(id -> ExpressionKind()) ++ aggExpr(expr) ++ aggStmt(body)
        case Do(body,cond) => aggStmt(body) ++ aggExpr(cond)
        case Synchronized(body,lock) => aggStmt(body) ++ aggExpr(lock)
        case Switch(stmts,expr) => aggStmts(stmts) ++ aggExpr(expr)
        case Switchcase(expr) => aggExpr(expr)
        case Break(name) => Env(name -> ExpressionKind())
        case Continue(name) => Env(name -> ExpressionKind())
        case Assert(expr) => aggExpr(expr)
        case Constructor(exprs) => aggExprs(exprs)
        case SuperConstructor(exprs) => aggExprs(exprs)
        case _ => Env()
    }

    def aggExprs(es:List[Expression]):env = es.map(aggExpr).foldLeft(Env()){_ ++ _}

    def aggExpr(e:Expression):env = e match {
        case Metavar(name) => Env(name -> ExpressionKind())
        case Method(expr,name,args) => aggExpr(expr) ++ Env(name -> ExpressionKind()) ++ aggExprs(args)
        case NamedMethod(expr,_,args) => aggExpr(expr) ++ aggExprs(args)
        case BinOp(l,r,_) => aggExpr(l) ++ aggExpr(r)
        case UnOp(expr,_) => aggExpr(expr)
        case Cast(expr,_) => aggExpr(expr)
        case New(_,args) => aggExprs(args)
        case ArrayInit(args) => aggExprs(args)
        case InstanceOf(_,expr) => aggExpr(expr)
        case JavaLiteral(_) => Env()
    }

    def aggType(t:TypePattern):env = t match {
        case TypeMetavar(name) => Env(name -> TypeKind())
        case ArraType(inner) => aggType(inner)
        case _ => Env()
    }
    
}

abstract sealed class Kind {}
case class TypeKind() extends Kind
case class ExpressionKind() extends Kind
case class StatementKind() extends Kind

case class KindCheckingException(msg:String) extends Exception(msg) {

}
