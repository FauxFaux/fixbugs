package fixbugs.mc

object FixBugsTestRunner {
  def main(args : Array[String]) : Unit = {
    List(new RefinerSpec,new EvalSpec).foreach(x => x.execute())
  }
}
