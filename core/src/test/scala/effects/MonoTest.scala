package sss.effects

import org.scalatest._
import prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class MonoTest extends FreeSpec with Matchers with Checkers {
  import scalaz._
  import Scalaz._
  import Mono._

  "Mono example should be able to" - {
    def incr: BigInt => BigInt = _ + 1
    def decr: BigInt => BigInt = _ - 1

    def computation: Eff[BigInt] = for {
      x <- ask
    } yield incr(x)

    "obtain dynamic value from environment" in {
      check { (n: BigInt) =>
        runReader(computation)(n) == incr(n)
      }
    }

    "alter dynamic value in environment" in {
      check { (n: BigInt) =>
        incr(decr(n)) == decr(incr(n))
        incr(decr(n)) == n
        runReader(local(decr)(computation))(n) == n
      }
    }

    "NOT obtain multiple dynamic values in environment" in {
      def computation: Eff[BigInt] = for {
        x <- ask
        y <- ask
      } yield (x + y)

      check { (n: BigInt) =>
        runReader(computation)(n) == n + n
      }
    }
  }
}
