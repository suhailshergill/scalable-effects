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

    def computation: Eff[Reader[BigInt]#τ, BigInt] = for {
      x <- ask
    } yield (incr(x))

    // the >>= being used below is Eff.>>=
    def computationNoFor: Eff[Reader[BigInt]#τ, BigInt] = ask >>= (
      x => Monad[Eff[Reader[BigInt]#τ, ?]].point(incr(x))
    )

    // {{{ archive: assist scala inference for monad syntax

    val MonadEff = Monad[Eff[Reader[BigInt]#τ, ?]]
    // now without adding special >>= and map, we can still manually invoke the
    // correct monad instance and use 'bind' and 'point' as normal
    def computationBind: Eff[Reader[BigInt]#τ, BigInt] = MonadEff.bind(ask)(
      x => MonadEff.point(incr(x))
    )
    // now if we just had a generic 'bind' function, that still doesn't work
    // because of the type annotations needed.
    def bindGen[M[_]: Monad, A, B](ma: M[A])(f: A => M[B]): M[B] = {
      implicitly[Monad[M]].bind(ma)(f)
    }
    def computationBindGen: Eff[Reader[BigInt]#τ, BigInt] = bindGen[Eff[Reader[BigInt]#τ, ?], BigInt, BigInt](ask)(
      x => MonadEff.point(incr(x))
    )
    // if `bind' is specialized for Eff, then things are somewhat more
    // reasonable
    def bindSp[R[_], A, B](ma: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] = {
      implicitly[Monad[Eff[R, ?]]].bind(ma)(f)
    }
    def computationBindSp: Eff[Reader[BigInt]#τ, BigInt] = bindSp(ask[BigInt])(
      x => MonadEff.point(incr(x))
    )

    // }}}

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
      def computation: Eff[Reader[BigInt]#τ, BigInt] = for {
        x <- ask[BigInt]
        y <- ask[BigInt]
      } yield (x + y)

      check { (n: BigInt) =>
        Poly.run(runReader(computation)(n)) == n + n
      }
    }

    "define/run a computation defined on different environment type" in {
      def print: Double => String = x => "PRINTING: " + x
      def computation: Eff[Reader[Double]#τ, String] = for {
        x <- ask
      } yield (print(x))

      check { (n: Double) =>
        Poly.run(runReader(computation)(n)) == print(n)
      }
    }
  }
}
