package fixbugs.core.ir

import org.scalatest._
import matchers._
import Parser._


class ParserTest extends Spec with ShouldMatchers {

  describe("expresssions") {
    println(parse(expression,"i++"))
  }
  
}