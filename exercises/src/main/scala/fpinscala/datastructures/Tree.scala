package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // 3.25 a function size that counts the number of nodes (leaves and branches) in a tree
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  // 3.26 a function maximum that returns the maximum element in a Tree[Int]
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // 3.27 a function depth that returns the maximum path length from the root of a tree to any leaf
  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
    }
  }

  // 3.28 a function map that that modifies each element in a tree with a given function
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  // 3.29a Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities
  def fold[A, B](t: Tree[A])(f: A => B)(combine: (B, B) => B): B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => combine(fold(l)(f)(combine), fold(r)(f)(combine))
    }
  }
  // 3.29b Re-implement size, maximum, depth, and map in terms of this more general function
  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => l + r)
  def maximum2(t: Tree[Int]): Int = fold(t)(identity)((l, r) => l max r)
  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => (l + 1) max (r + 1))
  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A,Tree[B]](t)(a => Leaf(f(a)))((l, r) => Branch(l, r))
}