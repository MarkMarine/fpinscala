package fpinscala.laziness

import Stream._

trait Stream[+A] {

  // The arrow `=>` in front of the argument type `B` means that the function
  // `f` takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  // Here `b` is the unevaluated recursive step that folds the tail of
  // the stream. If `p(a)` returns `true`, `b` will never be evaluated and the
  // computation terminates early.
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) =>
    if (p(h)) cons(h, t)
    else empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true) {
    p(_) && _
  }

  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty)((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty)((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h) append t)

  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile(_._2.isDefined).forAll { case (h1, h2) => h1 == h2 }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case c => Some(c, c drop 1)
    } append Stream(empty)
  }

  def hasSubsequence[A](s: Stream[A]): Boolean  = tails exists(_ startsWith s)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = foldRight(empty[B]){
    (h,t) =>
  }

  def toList: List[A] = foldRight(List[A]()) {
    _ :: _
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }
  }

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
    case (Empty, _) => None
    case (_, Empty) => None
  }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A, B)] = zipWith(s2)((_, _))

  // continue the traversal as long as either stream has more elements, using
  // option to indicate the stream has been exhausted
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
    case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), empty))
    case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (empty, tb()))
    case _ => None
  }


}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // takes an inital state, and a function for producing both the next state
  // and the next value in the generated stream
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((aa, ss)) => cons(aa, unfold(ss)(f))
      case None => empty
    }
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  val fibs: Stream[Int] = {
    def inner(f0: Int, f1: Int): Stream[Int] =
      cons(f0, inner(f1, f0 + f1))

    inner(0, 1)
  }

  val fibs1: Stream[Int] = {
    case class St(f0: Int, f1: Int)
    def inner(s: St): Option[(Int, St)] = {
      s match {
        case St(a, b) if a <= 0 | b <= 0 => None
        case St(a, b) => Some(a + b, St(b, a + b))
      }
    }

    unfold(St(0, 1))(inner)
  }

  def from1(n: Int): Stream[Int] = {
    unfold(n) { x: Int => Some((x + 1, x + 1)) }
  }

  def constant1[A](v: A): Stream[A] = {
    unfold(v) { _ => Some(v, v) }
  }

  def ones1(): Stream[Int] = {
    unfold(1) { _ => Some(1, 1) }
  }


}