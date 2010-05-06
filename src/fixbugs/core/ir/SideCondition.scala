package fixbugs.core.ir

sealed abstract class SideCondition {}
case class Satisfies(name:String,cond:NodeCondition) extends SideCondition
case class SAnd(left:SideCondition,right:SideCondition) extends SideCondition
case class SOr(left:SideCondition,right:SideCondition) extends SideCondition
case class SNot(phi:SideCondition) extends SideCondition
case class STrue() extends SideCondition
case class SFalse() extends SideCondition
case class Subtypes(parent:TypePattern,child:TypePattern) extends SideCondition
case class Interface(name:String) extends SideCondition

