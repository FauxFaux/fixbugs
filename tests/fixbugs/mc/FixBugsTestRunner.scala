package fixbugs.mc

object FixBugsTestRunner {
  def main(args : Array[String]) : Unit = {
    (new EvalSpec).execute()
  }
}
