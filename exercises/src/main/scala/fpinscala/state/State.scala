package fpinscala.state

import scala.annotation.tailrec


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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 6.1
  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = { // could be absurdly unlucky and loop 'forever'
    val (i, rng2) = rng.nextInt
    if (i == Int.MinValue) nonNegativeInt(rng2)
    else if (i < 0) (-i, rng2)
    else (i, rng2)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue.toDouble, rng2)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = nonNegativeInt(rng2)
    ((d, i), rng3)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = if (count == 0) (List(), rng)
  else {
    val (i, rng2) = rng.nextInt
    val (l, rng3) = ints(count - 1)(rng2)
    (i :: l, rng3)
  }

  // 6.5 double via map
  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue.toDouble)

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((randA, bs) => map2(randA, bs)((a, bs) => a :: bs))
  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }
  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThanViaFlatMap(n)
  }

  // 6.9 map and map2 via flatMap
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}

case class State[S,+A](run: S => (A, S)) {
  // 6.10 generalize a bunch of earlier functions
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    })
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })

  def get: State[S, S] = State(s => (s, s))
  def set(s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

/**
 * The machine has two types of input: you can insert a coin, or you can turn the knob to dispense candy. It can be in
 * one of two states: locked or unlocked. It also tracks how many candies are left and how many coins it contains.
 *
 * The rules of the machine are as follows:
 * Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
 * Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
 * Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
 * A machine that’s out of candy ignores all inputs.
 */
case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  // 6.10 continued
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((randA, bs) => randA.map2(bs)((a, bs) => a :: bs))

  type Rand[A] = State[RNG, A]

  /**
   * simulateMachine should operate the machine based on the list of inputs and return the number of coins and candies
   * left in the machine at the end.
   * For example, if the input Machine has 10 coins and 5 candies, and a total of 4 candies are successfully bought,
   * the output should be (14, 1).
   */
  // 6.11
  // QUESTION: what's the nicer way to write this using foldRight and map/flatMap/get/set ?
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    def operateMachine(input: Input)(machine: Machine): Machine = input match {
      // inserting a coin into a locked machine will cause it to unlock if there's any candy left
      case Coin if machine.locked && machine.candies > 0 => machine.copy(locked = false, coins = machine.coins + 1)
      // turning knob on an unlocked machine will cause it to dispense candy and become locked
      case Turn if !machine.locked => machine.copy(locked = true, candies = machine.candies - 1)
      case Turn if machine.locked => machine // turning knob on locked machine does nothing
      // inserting a coin into an unlocked machine does nothing (but presumably still eats the coin)
      case Coin if !machine.locked => machine.copy(coins = machine.coins + 1)
      case _ if machine.candies == 0 => machine // machine out of candy ignores all inputs
    }

    val initial = State[Machine, (Int, Int)](s => ((s.coins, s.candies), s))
    inputs.foldLeft(initial)((s, input) => State(in => {
      val (_, machine) = s.run(in)
      val updatedM = operateMachine(input)(machine)
      ((updatedM.coins, updatedM.candies), updatedM)
    }))
  }
}
