package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B, C](t: Tree[A])(f: A => B)(comb: (B,B) => C): C = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l,r) => fold(l)(unit)(f) fold(r)(unit)(f)
    }
  }
}