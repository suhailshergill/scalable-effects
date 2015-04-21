package sss.effects

object Mono {
  import scalaz._
  import Scalaz._

  sealed trait VE[w]
  case class Val[w](x: w) extends VE[w]
  case class E[w](k: Int => VE[w]) extends VE[w]

  trait Eff[a] {
    def runEff[w]: (a => VE[w]) => VE[w]
  }

  implicit val monadEffInstance = new Monad[Eff] {
    def point[A](x: => A): Eff[A] = new Eff[A] {
      def runEff[w] = k => k(x)
    }
    def bind[A, B](fa: Eff[A])(f: A => Eff[B]): Eff[B] = new Eff[B] {
      def runEff[w] = k => fa.runEff(v => f(v).runEff(k))
    }
  }

  def ask = new Eff[Int] {
    def runEff[w] = k => E(k)
  }

  def admin[w]: Eff[w] => VE[w] = x => x.runEff(Val.apply)

  def runReader[w]: Eff[w] => Int => w = m => e => {
    def loop: VE[w] => w = y => y match {
      case Val(x) => x
      case E(k)   => loop(k(e))
    }
    loop(admin(m))
  }

  def local[w]: (Int => Int) => Eff[w] => Eff[w] = f => m => {
    ask >>= (e0 => {
      val e = f(e0)
      def loop: (VE[w] => Eff[w]) = y => y match {
        case Val(x) => Monad[Eff].pure(x)
        case E(k)   => loop(k(e))
      }
      loop(admin(m))
    })
  }

}
