package fixbugs.core.ir
import scala.collection.mutable.{Map => MMap,HashMap}

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

    def check(t:Transformation) = {
        val Replace(from,to,cond) = t
        val (fromEnv,toEnv) = (aggStmt(from),aggStmt(to))
        
        val names = Set() ++ toEnv.keys ++ fromEnv.keys
        if (names contains "_from")
            throw new KindCheckingException("You may not use '_from' as a variable name")

        val unbound = (Set() ++ toEnv.keys) -- (Set() ++ fromEnv.keys)
        if (! unbound.isEmpty)
            throw new KindCheckingException("The following variables are unbound (ie used in replacement, but not creation"+unbound)
        ()
    }

    def aggStmts(stmts:List[Statement]):env = stmts.map(aggStmt).foldLeft(Env()){_ ++ _}

    def aggStmt(stmt:Statement):env = stmt match {
        case Label(name,inner) => Env(name -> StatementKind()) ++ aggStmt(inner)
        case Assignment(typee,name,expr) => Env(name -> StatementKind()) ++ aggType(typee) ++ aggExpr(expr)
        case IfElse(cond,then,otherwise) => aggExpr(cond) ++ aggStmt(then) ++ aggStmt(otherwise)
        case While(cond,body) => aggExpr(cond) ++ aggStmt(body)
        case TryCatchFinally(tri,catc,finall) => aggStmt(tri) ++ aggStmt(catc) ++ aggStmt(finall)
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
        case Method(_,args) => aggExprs(args)
        case BinOp(l,r,_) => aggExpr(l) ++ aggExpr(r)
        case UnOp(expr,_) => aggExpr(expr)
        case Cast(expr,_) => aggExpr(expr)
        case New(_,args) => aggExprs(args)
        case ArrayInit(args) => aggExprs(args)
        case InstanceOf(_,expr) => aggExpr(expr)
    }

    def aggType(t:TypePattern):env = t match {
        case TypeMetavar(name) => Env(name -> TypeKind())
        case _ => Env()
    }
    
}

abstract sealed class Kind {}
case class TypeKind() extends Kind
case class ExpressionKind() extends Kind
case class StatementKind() extends Kind

class KindCheckingException(msg:String) extends Exception(msg) {

}
