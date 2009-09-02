package fixbugs.core.ir

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
    type env = Map[String,Kind]
    def Env():env = Map[String,Kind]()

    def check(t:Transformation) = {
        val Replace(from,to,cond) = t
        ()
    }

    def aggStmt(stmt:Statement):env = Map()

    def aggExprs(es:List[Expression]):env = es.map(aggExpr).foldLeft(Env()){(acc,m) => acc ++ m}

    def aggExpr(e:Expression):env = e match {
        case Metavar(name) => Map(name -> Expression())
        case Method(_,args) => aggExprs(args)
        case BinOp(l,r,_) => aggExpr(l) ++ aggExpr(r)
        case UnOp(expr,_) => aggExpr(expr)
        case Cast(expr,_) => aggExpr(expr)
        case New(_,args) => aggExprs(args)
        case ArrayInit(args) => aggExprs(args)
        case InstanceOf(_,expr) => aggExpr(expr)
    }

    def aggType(t:TypePattern):env = t match {
        case TypeMetavar(name) => Map(name -> Type())
        case _ => Env()
    }
    
}

abstract sealed class Kind {}
case class Type() extends Kind
case class Expression() extends Kind
case class Statement() extends Kind

class KindCheckingException(msg:String) extends Exception(msg) {

}
