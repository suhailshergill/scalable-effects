package sss.effects

import org.scalatest._
import prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class PolyTest extends FreeSpec with Matchers with Checkers {
  import scalaz.{ Reader => _, _ }
  import Scalaz._
  import Poly._
  import Reader._

  "Poly example should be able to" - {
    def incr: BigInt => BigInt = _ + 1
    def decr: BigInt => BigInt = _ - 1

    def computation: Eff[Reader[BigInt]#t0]#t0[BigInt] = for {
      x <- ask[BigInt]
    } yield (incr(x))

    def computationNoFor: Eff[Reader[BigInt]#t0]#t0[BigInt] = ask[BigInt] >>= (
      x => Monad[Eff[Reader[BigInt]#t0]#t0].pure(incr(x))
    )

    "obtain dynamic value from environment" in {
      check { (n: BigInt) =>
        Poly.run(runReader(computation)(n)) == incr(n)
      }
    }

    "alter dynamic value in environment" in {
      check { (n: BigInt) =>
        incr(decr(n)) == decr(incr(n))
        incr(decr(n)) == n
        Poly.run(runReader(local(decr)(computation))(n)) == n
      }
    }

    "NOT obtain multiple dynamic values in environment" in {
      def computation: Eff[Reader[BigInt]#t0]#t0[BigInt] = for {
        x <- ask[BigInt]
        y <- ask[BigInt]
      } yield (x + y)

      check { (n: BigInt) =>
        Poly.run(runReader(computation)(n)) == n + n
      }
    }

    "define/run a computation defined on different environment type" in {
      def print: Double => String = x => "PRINTING: " + x
      def computation: Eff[Reader[Double]#t0]#t0[String] = for {
        x <- ask[Double]
      } yield (print(x))

      check { (n: Double) =>
        Poly.run(runReader(computation)(n)) == print(n)
      }
    }
  }
}
