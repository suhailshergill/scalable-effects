package sss.effects

import org.scalatest._

class MacroTest extends FreeSpec with Matchers {
  import MacroDef._
  "A macro definition" - {
    "should be invocable in a separate compilation unit" in {
      printf("hello %s!", "world")
    }
  }
}
