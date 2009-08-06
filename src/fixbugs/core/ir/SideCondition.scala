package fixbugs.core.ir

sealed abstract class SideCondition {}
case class Satisfies(name:String,cond:NodeCondition) extends SideCondition
case class SAnd(left:SideCondition,right:SideCondition) extends SideCondition
case class SOr(left:SideCondition,right:SideCondition) extends SideCondition
case class SNot(phi:SideCondition) extends SideCondition