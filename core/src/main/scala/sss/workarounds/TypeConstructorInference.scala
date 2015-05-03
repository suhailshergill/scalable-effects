package sss.workarounds

object TypeConstructorInference {
  // https://issues.scala-lang.org/browse/SI-6744

  trait Process[F[_, _], A]
  case class Halt[F[_, _], A]() extends Process[F, A]
  case class Emit[F[_, _], A](head: A, tail: Process[F, A]) extends Process[F, A]
  case class Await[F[_, _], X, Y, A](fn: F[X, Y], arg: X, recv: Y => Process[F, A])
    extends Process[F, A]

  object Failing {
    trait F1[A, B]
    case class Get[B]() extends F1[Unit, B]
    trait One[A] {
      type f[X, Y] = F1[Unit, A]
    }

    // I'm guessing this is just the usual limitation that a type
    // constructor will never be inferred as a type level function
    val one = Get[Int]()
    val p: Process[One[Int]#f, String] =
      Await[One[Int]#f, Unit, Int, String](one, (), (i: Int) => Halt[One[Int]#f, String]())
    // val r = p match { case Await(f, x, r) => f }

    // [error] /home/shergill/virtualEnvs/extensible-effects/scalable-effects/core/src/main/scala/sss/effects/Poly.scala:109: constructor cannot be instantiated to expected type;
    // [error]  found   : sss.effects.Poly.Reader.error.Await[F,X,Y,A]
    // [error]  required: sss.effects.Poly.Reader.error.Await[[X, Y]sss.effects.Poly.Reader.error.F1[Unit,Int],Unit,Int,String]
    // [error]         val r = a1 match { case Await(f, x, r) => f }
  }

  // {{{ archive

  object Succeeding2 {
    case class Two[A, B]() { // a two input Process
      trait f[x, y] { def get: Either[A =:= y, B =:= y] }
      case object L extends f[Unit, A] { def get = Left(implicitly) }
      case object R extends f[Unit, B] { def get = Right(implicitly) }
    }
    val a1 = Await[Two[Unit, Int]#f, Unit, Int, String](Two[Unit, Int]().R, (), (i: Int) => Halt[Two[Unit, Int]#f, String]())
    val x: Process[Two[Unit, Int]#f, String] = a1
    val r = a1 match { case Await(f, x, r) => f }
    // I then use Process[Two[A,B]#f,C] to represent a Process that accepts
    // two inputs, and now f is an actual type, not just a type alias, so I
    // can go back to pattern matching on Await. I can't pattern match on L
    // and R (since Two[A,B]().L != Two[A,B]().L - they belong to completely
    // separate types), but even if I could Scala is missing the GADT
    // support that would refine the types properly there anyway. Instead I
    // pattern match on the result of calling get, which contains an
    // equality witness on either side that also lets me refine the types
    // correctly. Somewhat annoying to use but not too horrible so far...
  }

  // }}}

  object Succeeding {
    case class One[A]() {
      trait f[X, Y] { def get: Y =:= A }
      // the below is taking the place of the Get. this is because we can't
      // extend an inner without reference to an outer. the `get' allows us
      // to pattern match since it holds a type equality witness.
      case object t extends f[Unit, A] { def get = implicitly }
    }
    val one = One[Int]()
    val p: Process[One[Int]#f, String] =
      Await[One[Int]#f, Unit, Int, String](one.t, (), (i: Int) => Halt[One[Int]#f, String]())
    val r = p match { case Await(f, x, r) => f }
  }
}
