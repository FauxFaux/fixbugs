package fixbugs.mc

import org.scalacheck._

import java.util.Random

object FixbugsSpecification extends Properties("Fixbugs") {

  include(RefinerSpecification)
  //include(EvalSpecification)
  
  override def main(args:Array[String]) {
    check(Test.Params(100,5,1,17,new Random,1,1))
  }
}
