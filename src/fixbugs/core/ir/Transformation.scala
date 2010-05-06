package fixbugs.core.ir

case class VDecl(tp:TypePattern,mv:Metavar)

sealed abstract class Transformation {}
case class Replace(from:Statement,to:Statement,cond:SideCondition) extends Transformation {
    override def toString() = "from: %s\nto: %s\ncond: %s".format(from,to,cond)
}
case class AddMethod(ret:VDecl,args:List[VDecl],stmts:List[Statement],named:Metavar,cond:SideCondition) extends Transformation
case class Then(trans:List[Transformation]) extends Transformation
case class Pick(trans:List[Transformation]) extends Transformation
