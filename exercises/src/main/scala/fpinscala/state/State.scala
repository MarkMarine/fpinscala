package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, nRng) = rng.nextInt
    (if (x < 0) -(x + 1) else x, nRng)
  }

  def double(rng: RNG): Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nRng) = rng.nextInt
    val (d, nnRng) = double(nRng)
    ((i, d), nnRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, nRng) = double(rng)
    val (i, nnRng) = nRng.nextInt
    ((d, i), nnRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d0, rng0) = double(rng)
    val (d1, rng1) = double(rng0)
    val (d2, rng2) = double(rng1)
    ((d0, d1, d2), rng2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    ???
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rn) = ra(rng)
    val (b, rnn) = rb(rn)
    (f(a, b), rnn)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]]

  def sequence1[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]())) {
    (f, acc) =>
      //      map2(f, acc)(_ :: _)
    { rng =>
      val (a, nrng) = f(rng)
      val (b, retRng) = acc(nrng)
      (a :: b, retRng)
    }
  }

  //  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
  //    def loop(ls: List[Rand[A]])(f: (List[A], A) => List[A] ): Rand[List[A]] = {
  //      ls match {
  //        case Nil => unit(List[A]())
  //        case
  //      }
  //    }


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, newRng) = f(rng)
    g(a)(newRng)
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a => unit(f(a)) }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { aa => map(rb)(bb => f(aa, bb)) }


}

case class State[S, +A](run: S => (A, S)) {

  import State._

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(f(a, _)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, ns) = run(s)
    f(a).run(ns)
  })
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

/*
The machine has two types of input:
- insert a coin
- turn the knob to dispense

The machine can be in one of two states:
- locked
- unlocked

It also tracks how many coins it contains and candies are left

The rules are as follows:
- inserting a coin when locked will cause it to unlock if there is any candy left
- turning the knob on an unlocked machine will cause it to dispense candy and lock
- turning the knob on a locked machine or machine that is out of candy ignores all inputs
*/

object Candy {
  import State._

  def update() = (i: Input) => { m: Machine =>
    (i,m) match {
      case (Coin, Machine(true, c, $)) if c > 0 => Machine(false, c, $ + 1)
      case (Turn, Machine(false, c, $)) => Machine(true, c - 1, $)
      case _ => m
    }
  }
  def get: S => A
  def set: (S, A) => S

  def modify[S](f: Input => Machine): Machine

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map(fi => modify[Machine] fi compose update))
    s <- get
  } yield ()
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
    ss.foldRight(unit[S, List[A]](List[A]())) { (f, acc) => f.map2(acc)(_ :: _)
    }


}






