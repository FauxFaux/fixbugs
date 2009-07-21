package fixbugs.mc


/**
 * Paths within Logic
 */
sealed abstract class Path {}

/**
 * CTL core
 */
case class Next(cond:NodeCondition) extends Path
case class Until(phi:NodeCondition,psi:NodeCondition) extends Path

/**
 * CTL Sugar
 */
case class Future(cond:NodeCondition) extends Path
case class Global(cond:NodeCondition) extends Path

sealed abstract class NodeCondition {}

/**
 * Temporal operators
 */
case class All(path:Path) extends NodeCondition
case class Exists(path:Path) extends NodeCondition

/**
 * Predicates
 */
case class LineNumber(number:Int) extends NodeCondition