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
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

//  val x = List(1,2,3,4,5) match {
//    case Cons(x, Cons(2, Cons(4, _))) => x
//    case Nil => 42
//    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
//    case Cons(h, t) => h + sum(t)
//    case _ => 101
//  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // 3.1 is a mental exercise

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, xs) => xs
    case Nil => Nil
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, xs) => Cons(h, xs)
    case Nil => Nil
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Cons(_, xs) => drop(xs, n - 1)
      case Nil => Nil
    }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case all@Cons(a, as) => if (f(a)) dropWhile(as, f) else all
    case _ => Nil
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Cons(a, Cons(_, Nil)) => Cons(a, Nil)
    case Cons(a, Nil) => Nil
    case Cons(a, as) => Cons(a, init(as))
    case Nil => Nil
  }

  // 3.7 is a mental exercise

  // 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, size) => size + 1)

  // 3.10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(a, as) => foldLeft(as, f(z, a))(f)
  }

  // 3.11 - rewrite sum, product and length using foldLeft
  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)
  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((size, _) => size + 1)

  // 3.12 reverse a list using fold
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((as, a) => Cons(a, as))

  // 3.13a write foldLeft in terms of foldRight
  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(a, as) => f(foldRight(as, z)((x, y) => f(y, x)), a)
  }
  // 3.13b write foldRight in terms of foldLeft
  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(a, as) => foldLeft(as, f(a, z))((x, y) => f(y, x))
  }

  // 3.14 append in terms of foldLeft or foldRight
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a, as) => Cons(a, as))

  // 3.15 a function that concatenates a list of lists into a single list
  // QUESTION: Assuming foldRight and append are implemented sanely, is this correct?
  def flatten[A](ass: List[List[A]]): List[A] = foldRight(ass, Nil: List[A])((a, as) => append(a, as))

  // 3.16 a function that transforms a list of integers by adding 1 to each element.
  def addOneToEach(l: List[Int]): List[Int] = map(l)(_ + 1)

  // 3.17 a function that transforms a list of integers by adding 1 to each element.
  def doublesToStrings(l: List[Double]): List[String] = map(l)(_.toString)

  // 3.18 a function map that generalizes modifying each element in a list while maintaining the structure of the list
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, bs) => Cons(f(a), bs))

  // 3.19 a function filter that removes elements from a list unless they satisfy a given predicate
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((a, as) => if (f(a)) Cons(a, as) else as )

  // 3.20 a function flatMap that works like map except that the function given will return a list instead of a single
  // result, and that list should be inserted into the final resulting list
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((a, bs) => append(f(a), bs))

  // 3.21 implement filter with flatMap
  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) Cons(a, Nil) else Nil)

  // 3.22 a function that accepts two lists and constructs a new list by adding corresponding elements
  def addListsOfInts(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Cons(a, as), Cons(b, bs)) => Cons(a + b, addListsOfInts(as, bs))
    case _ => Nil
  }

  // 3.23 generalised form of addListsOfInts
  // QUESTION: is there a nicer way to write this using a fold?
  // A: seems like no, or at least the answers write it this way too.
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
    case _ => Nil
  }

  // 3.24 implement hasSubsequence for checking whether a List contains another List as a subsequence
  def hasSubsequence[A](big: List[A], sub: List[A]): Boolean = big match {
    case Nil => false
    case _ => length(filter(zipWith(big, sub)(_ == _))(identity)) == length(sub) || hasSubsequence(tail(big), sub)
  }
}
