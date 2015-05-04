package sss.effects

object Poly {
  import scalaz.{ Reader => _, _ }
  import Scalaz._

  // {{{ forall

  object Rank2 {
    // [[https://apocalisp.wordpress.com/2010/07/02/higher-rank-polymorphism-in-scala/][source]]
    object natural {
      trait ~>[F[_], G[_]] {
        def apply[A](a: F[A]): G[A]
      }

      type Id[A] = A
    }

    object forall {
      // [[http://stackoverflow.com/a/7216901][how to encode ∀ in scala]]
      type ∀[x[_]] = Forall[x]
    }
  }
  import Rank2.forall._

  // }}}

  sealed trait ValueEffect[w, r[_]]
  case class Value[w, r[_]](x: w) extends ValueEffect[w, r]
  case class Effect[w, r[_]](k: r[ValueEffect[w, r]]) extends ValueEffect[w, r]

  // {{{ Eff

  case class Eff[r[_]]() {
    // TODO: is `0` needed?
    trait `0`[r0[_], a] {
      def get: r0[_] =:= r[_]
    }
    trait t0[a] extends `0`[r, a] {
      def get = implicitly
      def runEff[w]: (a => ValueEffect[w, r]) => ValueEffect[w, r]
    }
    type RunEff[a] = ∀[λ[w => ((a => ValueEffect[w, r]) => ValueEffect[w, r])]]
    def apply[a]: RunEff[a] => t0[a] = f => new t0[a] {
      def runEff[w] = f.apply[w]
    }
  }

  // {{{ Functor instance

  // FIXME: turning the below into an implicit breaks things
  def functorEffInstance[r[_]]: Functor[Eff[r]#t0] = new Functor[Eff[r]#t0] {
    def map[A, B](fa: Eff[r]#t0[A])(f: A => B): Eff[r]#t0[B] =
      Eff[r]()[B](new Eff[r]#RunEff[B] {
        def apply[w] = g => fa.runEff[w](a => g(f(a)))
      })
  }

  // }}}
  // {{{ Monad instance

  implicit def monadEffInstance[r[_]]: Monad[Eff[r]#t0] = new Monad[Eff[r]#t0] {
    def point[A](a: => A): Eff[r]#t0[A] = Eff[r]()[A](new Eff[r]#RunEff[A] {
      def apply[w] = k => k(a)
    })
    def bind[A, B](fa: Eff[r]#t0[A])(f: A => Eff[r]#t0[B]): Eff[r]#t0[B] =
      Eff[r]()[B](new Eff[r]#RunEff[B] {
        def apply[w] = k => fa.runEff(v => f(v).runEff(k))
      })
  }

  // }}}

  // }}}

  // {{{ Eff: send, admin, run

  def send[r[_], a]: ∀[λ[w => ((a => ValueEffect[w, r]) => r[ValueEffect[w, r]])]] => Eff[r]#t0[a] =
    f => Eff[r]()[a](new Eff[r]#RunEff[a] {
      def apply[w] = (k: (a => ValueEffect[w, r])) => Effect(f.apply(k))
    })

  def admin[r[_], w]: Eff[r]#t0[w] => ValueEffect[w, r] = eff => eff.runEff(Value.apply)

  sealed trait Void[v]
  case class ImpossibleHappened(x: String) extends Exception(x)

  def run[w]: Eff[Void]#t0[w] => w = m => admin(m) match {
    case Value(x) => x
    case Effect(_) => throw ImpossibleHappened("""Eff[Void, ?] seems to be
                                               encoding an Effect""")
  }

  // }}}

  object Reader {

    case class Reader[e]() {
      // TODO: is `0` needed?
      trait `0`[i, o] { def get: i =:= e }
      case class t0[o](x: e => o) extends `0`[e, o] {
        def get = implicitly
      }
    }
    // {{{ archive: old Reader. see sss.workarounds.TypeConstructorInference

    // sealed trait Reader[e, v]
    // object Reader {
    //   case class ReaderImpl[e, v](x: e => v) extends Reader[e, v]
    // }

    // trait Foo[e] {
    //   type t[V] = Reader[e, V]
    //   type u[V] = Void[V]
    //   type v[V] = Tuple2[e, V]
    //   type x[V] = V
    //   type y[V] = e
    // }

    // }}}

    def ask[e]: Eff[Reader[e]#t0]#t0[e] = send[Reader[e]#t0, e](new ∀[λ[w => ((e => ValueEffect[w, Reader[e]#t0]) => Reader[e]#t0[ValueEffect[w, Reader[e]#t0]])]] {
      def apply[w] = Reader[e].t0.apply
    })

    def runReader[w, e]: Eff[Reader[e]#t0]#t0[w] => e => Eff[Void]#t0[w] =
      reader => env_init => {
        def loop: ValueEffect[w, Reader[e]#t0] => Eff[Void]#t0[w] = ve => ve match {
          case Value(x)  => Monad[Eff[Void]#t0].pure(x)
          case Effect(k) => loop(k.x(env_init))
        }
        loop(admin(reader))
      }

    def local[w, e]: (e => e) => Eff[Reader[e]#t0]#t0[w] => Eff[Reader[e]#t0]#t0[w] =
      env_xform => reader => {
        ask[e] >>= (env0 => {
          val env = env_xform(env0)
          def loop: ValueEffect[w, Reader[e]#t0] => Eff[Reader[e]#t0]#t0[w] = ve =>
            ve match {
              case Value(x)  => Monad[Eff[Reader[e]#t0]#t0].pure(x)
              case Effect(k) => loop(k.x(env))
            }
          loop(admin(reader))
        })
      }

  }
}
