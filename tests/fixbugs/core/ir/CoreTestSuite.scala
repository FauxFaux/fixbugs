package fixbugs.core.ir

object CoreTestSuite {
  def main(args : Array[String]) : Unit = {
    //List(new ParserTest).map(_.execute)
    List(new GeneratorTest).map(_.execute)
  }
}
