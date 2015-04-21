package sss.effects

object Mono {
  import scalaz._
  import Scalaz._

  sealed trait ValueEffect[w]
  case class Value[w](x: w) extends ValueEffect[w]
  case class Effect[w](k: BigInt => ValueEffect[w]) extends ValueEffect[w]

  trait Eff[a] {
    def runEff[w]: (a => ValueEffect[w]) => ValueEffect[w]
  }

  implicit val monadEffInstance = new Monad[Eff] {
    def point[A](x: => A): Eff[A] = new Eff[A] {
      def runEff[w] = k => k(x)
    }
    def bind[A, B](fa: Eff[A])(f: A => Eff[B]): Eff[B] = new Eff[B] {
      def runEff[w] = k => fa.runEff(v => f(v).runEff(k))
    }
  }

  def ask = new Eff[BigInt] {
    def runEff[w] = k => Effect(k)
  }

  def admin[w]: Eff[w] => ValueEffect[w] = eff => eff.runEff(Value.apply)

  def runReader[w]: Eff[w] => BigInt => w = reader => env_init => {
    def loop: ValueEffect[w] => w = valueEff => valueEff match {
      case Value(x)  => x
      case Effect(k) => loop(k(env_init))
    }
    loop(admin(reader))
  }

  def local[w]: (BigInt => BigInt) => Eff[w] => Eff[w] = env_xform => reader => {
    ask >>= (env0 => {
      val env = env_xform(env0)
      def loop: (ValueEffect[w] => Eff[w]) = valueEff => valueEff match {
        case Value(x)  => Monad[Eff].pure(x)
        case Effect(k) => loop(k(env))
      }
      loop(admin(reader))
    })
  }

}
