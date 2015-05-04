package sss.workarounds

import org.scalatest._

class TypeConstructorInferenceTest extends FreeSpec {
  import TypeConstructorInference._
  import Succeeding._

  "nested types in classes are" - {
    "unique per instance" in {
      val x = One[Int]()
      assertResult(x.t)(x.t)
      assert(x.t != One[Int]().t)
    }
  }

  "case class with partially applied type constructor can be" - {
    "pattern matched" in {
      assertResult(one.t)(r)
    }
  }

}
