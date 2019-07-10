package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = {
    case Some(a) if f(a) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m => mean(xs.map { x => math.pow(x - m, 2) }) }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap { aa: A => b.map { bb: B => f(aa, bb) } }

  def map22[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    aa <- a
    bb <- b
  } yield f(aa,bb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => None
      case x :: xs => x flatMap { xh => sequence(xs) map (xh :: _) }

    }
  }


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]]) { (h, t) => map2(f(h), t)(_ :: _) }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a){ x => x }
  }
}