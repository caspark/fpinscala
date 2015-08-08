package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight(List[A]())(_ :: _)

  def take(n: Int): Stream[A] =
    if (n == 0) Empty
    else this match {
      case Cons(x, xs) => cons(x(), xs().take(n - 1))
      case Empty => Empty
    }

  def drop(n: Int): Stream[A] =
    if (n == 0) this
    else this match {
      case Cons(_, xs) => xs().drop(n - 1)
      case Empty => Empty
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(x, xs) if p(x()) => cons(x(), xs().takeWhile(p))
      case _ => Empty
    }

  // 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(x, xs) => p(x()) && xs().forAll(p)
    case _ => true
  }

  // 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  // 5.6
  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight.
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))
  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)
  def append[AA >: A](x: => Stream[AA]): Stream[AA] = foldRight(x)(cons(_, _))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a) append b)

  // 5.13 Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some(f(h()), t())
  }
  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Empty, _) | (_, 0) => None
    case (Cons(h, t), i) => Some((h(), (t(), i)))
  }
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }
  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(a, as), Cons(b, bs)) => Some(f(a(), b()), (as(), bs()))
    case _ => None
  }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(a, as), Cons(b, bs)) => Some((Some(a()), Some(b())), (as(), bs()))
    case (Cons(a, as), Empty) => Some(((Some(a()), None), (as(), empty)))
    case (Empty, Cons(b, bs)) => Some((None, Some(b())), (empty, bs()))
    case _ => None
  }

  // 5.14
  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).foldRight(true)((ab, r) => ab match {
    case (Some(a), Some(b)) => r && a == b
    case (_, None) => r
    case _ => false
  })
  def startsWithUsingHint[B](s: Stream[B]): Boolean = zipAll(s).takeWhile {
    case (_, Some(_)) => true
    case _ => false
  }.forAll(ab => ab._1 == ab._2)

  // 5.15 tails using unfold, which returns the Stream of suffixes of the input sequence, starting with the original Stream
  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((Cons(h, t), t()))
  }

  // 5.16 Generalize tails to the function scanRight, which is like a foldRight that returns a stream of the intermediate results
  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    foldRight((Stream(z), z)) {
      case (a, (sb, b)) => (cons(f(a, b), sb), f(a, b))
    }._1
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

  // 5.8 Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // 5.9 a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on.
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // 5.10 a function fibs that generates the infinite stream of Fibonacci numbers
  def fibs: Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] = cons(n1 + n2, go(n2, n1 + n2))
    cons(0, cons(1, go(0, 1)))
  }

  // 5.11 a more general stream-building function called unfold
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map{ case (a, s) => cons(a, unfold(s)(f)) }.getOrElse(empty)

  // 5.12 fibs, from, constant, and ones in terms of unfold
  val onesViaUnfold: Stream[Int] = unfold(())(_ => Some((1, ())))
  def constantViaUnfold[A](z: A): Stream[A] = unfold(())(_ => Some((z, ())))
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(i => Some((i, i + 1)))
  def fibsViaUnfold: Stream[Int] = Stream(0, 1).append(unfold((0, 1)){ case (n1, n2) => Some((n1 + n2, (n2, n1 + n2))) })
}