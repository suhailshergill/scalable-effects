package sss.effects

import org.scalatest._
import prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class MonoTest extends FreeSpec with Matchers with Checkers {
  import scalaz._
  import Scalaz._
  import Mono._

  "Mono example should" - {
    def incr: Int => Int = _ + 1
    def decr: Int => Int = _ - 1

    def computation: Eff[Int] = for {
      x <- ask
    } yield incr(x)

    "be able obtain dynamic value from environment" in {
      check { (n: Int) =>
        runReader(computation)(n) == incr(n)
      }
    }

    "be able to alter dynamic value in environment" in {
      check { (n: Int) =>
        incr(decr(n)) == decr(incr(n))
        incr(decr(n)) == n
        runReader(local(decr)(computation))(n) == n
      }
    }
  }
}
