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

  trait Eff[r[_], a] { self =>
    def runEff[w]: (a => ValueEffect[w, r]) => ValueEffect[w, r]

    // FIXME:
    // the below are needed to help scala's inference engine. essentially the
    // scalaz implicits wihch are based off generic monad M[_]'s don't seem to
    // be sufficient when M[_] =:=  Eff[r, ?]
    // [x] it doesn't seem to be due to kind projector (see monadEffInstance2)
    implicit def M = implicitly[Monad[Eff[r, ?]]]
    // the >>= below is needed for using the operator with Eff
    // the fact this is needed implies that the implicit for
    // BindOps[Eff[r, ?], a] in scalaz.syntax.Syntaxes.monad isn't firing.
    // [ ] what if we were to specialize it?
    def >>=[b]: (a => Eff[r, b]) => Eff[r, b] = M.bind(self)
    // 'map' below is needed to enable for comprehensions
    def map[B]: (a => B) => Eff[r, B] = M.map(self)
  }

  // {{{ archive: fix BindOps implicits

  // import syntax.BindOps
  // implicit def monadEffBindOps[r[_], a]: BindOps[Eff[r, ?], a] = new BindOps[Eff[r, ?], a] {
  //   def F: Bind[Eff[r, ?]] = implicitly
  //   def self: Eff[r, a] = ???
  // }
  // def unapplyEff[TC[_[_]], R[_], A0](implicit TC0: TC[Eff[R, ?]]): Unapply[TC, Eff[R, A0]] {
  //   type M[X] = Eff[R, X]
  //   type A = A0
  // } = new Unapply[TC, Eff[R, A0]] {
  //   type M[X] = Eff[R, X]
  //   type A = A0
  //   def TC = TC0
  //   def leibniz = Leibniz.refl
  // }
  // implicit def unapplyEffBind[R[_], A0](implicit TC0: Bind[Eff[R, ?]]): Unapply[Bind, Eff[R, A0]] {
  //   type M[X] = Eff[R, X]
  //   type A = A0
  // } = unapplyEff[Bind, R, A0](monadEffInstance)
  // object Eff {
  //   trait t0[r[_]] {
  //     type t1[x] = Eff[r, x]
  //   }
  // }

  // }}}

  // {{{ Functor instance

  implicit def functorEffInstance[r[_]]: Functor[Eff[r, ?]] = new Functor[Eff[r, ?]] {
    def map[A, B](fa: Eff[r, A])(f: A => B): Eff[r, B] = new Eff[r, B] {
      def runEff[w] = g => fa.runEff[w](a => g(f(a)))
    }
  }

  // }}}
  // {{{ Monad instance

  // implicit def monadEffInstance2[r[_]]: Monad[Eff.t0[r]#t1] = new Monad[Eff.t0[r]#t1] {
  implicit def monadEffInstance[r[_]]: Monad[Eff[r, ?]] = new Monad[Eff[r, ?]] {
    def point[A](a: => A): Eff[r, A] = new Eff[r, A] {
      def runEff[w] = k => k(a)
    }
    def bind[A, B](fa: Eff[r, A])(f: A => Eff[r, B]): Eff[r, B] = new Eff[r, B] {
      def runEff[w] = k => fa.runEff(v => f(v).runEff(k))
    }
  }

  // }}}

  // }}}

  // {{{ Eff: send, admin, run

  def send[r[_], a]: ∀[λ[w => ((a => ValueEffect[w, r]) => r[ValueEffect[w, r]])]] => Eff[r, a] =
    f => new Eff[r, a] {
      def runEff[w] = (k: (a => ValueEffect[w, r])) => Effect(f.apply(k))
    }

  def admin[r[_], w]: Eff[r, w] => ValueEffect[w, r] = eff => eff.runEff(Value.apply)

  sealed trait Void[v]
  case class ImpossibleHappened(x: String) extends Exception(x)

  def run[w]: Eff[Void, w] => w = m => admin(m) match {
    case Value(x) => x
    case Effect(_) => throw ImpossibleHappened("""Eff[Void, ?] seems to be
    encoding an Effect""")
  }

  // }}}

  object Reader {

    // {{{ archive: unapplyEffReader

    // [error]
    // /home/shergill/virtualEnvs/extensible-effects/scalable-effects/core/src/test/scala/effects/PolyTest.scala:30:
    // Implicit not found:

    def unapplyEffReader[TC[_[_]], A0, B0](implicit TC0: TC[Eff[Reader[B0]#τ, ?]]): Unapply[TC, Eff[Reader[B0]#τ, A0]] {
      type M[X] = Eff[Reader[B0]#τ, X]
      type A = A0
    } = ???

    // scalaz.Unapply[scalaz.Bind, sss.effects.Poly.Eff[sss.effects.Poly.Reader.Reader[e]#τ,e]].
    // Unable to unapply type `sss.effects.Poly.Eff[sss.effects.Poly.Reader.Reader[e]#τ,e]` into a type constructor of kind `M[_]` that is classified by the type class `scalaz.Bind`. Check that the type class is defined by compiling `implicitly[scalaz.Bind[type constructor]]` and review the implicits in object Unapply, which only cover common type 'shapes.'

    // }}}

    def ask[e]: Eff[Reader[e]#τ, e] = send[Reader[e]#τ, e](new ∀[λ[w => ((e => ValueEffect[w, Reader[e]#τ]) => Reader[e]#τ[ValueEffect[w, Reader[e]#τ]])]] {
      def apply[w] = Reader[e].τ.apply
    })

    case class Reader[e]() {
      trait Inner[i, o] { def get: i =:= e }
      case class τ[o](x: e => o) extends Inner[e, o] {
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

    def runReader[w, e]: Eff[Reader[e]#τ, w] => e => Eff[Void, w] =
      reader => env_init => {
        def loop: ValueEffect[w, Reader[e]#τ] => Eff[Void, w] = ve => ve match {
          case Value(x)  => Monad[Eff[Void, ?]].pure(x)
          case Effect(k) => loop(k.x(env_init))
        }
        loop(admin(reader))
      }

    def local[w, e]: (e => e) => Eff[Reader[e]#τ, w] => Eff[Reader[e]#τ, w] =
      env_xform => reader => {
        ask >>= (env0 => {
          val env = env_xform(env0)
          def loop: ValueEffect[w, Reader[e]#τ] => Eff[Reader[e]#τ, w] = ve =>
            ve match {
              case Value(x)  => Monad[Eff[Reader[e]#τ, ?]].pure(x)
              case Effect(k) => loop(k.x(env))
            }
          loop(admin(reader))
        })
      }

  }
}
