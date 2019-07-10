package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2) {
    Cons(_, _)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(xh, xs) => f(xh, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if n <= 0 => Cons(h, t)
    case Cons(_, t) if n > 0 => drop(t, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    def initInner(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(h, t) => initInner(t, Cons(h, acc))
    }

    def reverse(l: List[A]): List[A] = {
      def revInner(ll: List[A], acc: List[A]): List[A] = {
        ll match {
          case Nil => acc
          case Cons(h, t) => revInner(t, Cons(h, acc))
        }
      }

      revInner(l, Nil)
    }

    reverse(initInner(l, Nil))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0) { (_, b) => b + 1 }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(xh, xs) => foldLeft(xs, f(z, xh))(f)
  }

  def sum3[A <: Int](ns: List[A]) = foldLeft(ns, 0)(_ + _)

  def product3[A <: Double](ns: List[A]) = foldLeft(ns, 1.0)(_ * _)

  def length3[A](l: List[A]): Int = foldLeft(l, 0) { (acc, _) => acc + 1 }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A]) { (b, a) => Cons(a, b) }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, List[B]())((a, b) => Cons(f(a), b))
  }

  def flatten[A](ls: List[List[A]]): List[A] = foldRight(ls, List[A]())(append)

  def addOne(ls: List[Int]): List[Int] = ls match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, addOne(t))
  }

  def addOne2(ls: List[Int]): List[Int] = foldRight(ls, List[Int]())((a, b) => Cons(a + 1, b))

  def toS(ds: List[Double]): List[String] = foldRight(ds, List[String]())((a: Double, b: List[String]) => Cons(a.toString, b))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List[B]())((a, b) => append(f(a), b))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l) { a => if (f(a)) Cons(a, Nil) else Nil }
  }

  def add2Lists(a1: List[Int], a2: List[Int]): List[Int] = {
    (a1, a2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(ah, at), Cons(bh, bt)) => Cons(ah + bh, add2Lists(at, bt))
    }
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    (a,b) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
    }
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {

    val f = (a: A, b: A) => a == b

    (l, sub) match {
      case (_, Nil) => true // todo... this means the pattern is exhausted - what does that mean
      case (Nil, _) => false
      case (Cons(ah, at), Cons(bh, bt)) if f(ah, bh) => hasSubsequence(at, bt)
    }
  }
}
