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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def double2: Rand[Double] =
    map(nonNegativeInt)(x => x / (Int.MaxValue.toDouble + 1))


  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r2) = int(rng)
    val (d, r3) = double(r2)
    ((i, d), r3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r2) = double(rng)
    val (d2, r3) = double(r2)
    val (d3, r4) = double(r3)
    ((d1, d2, d3), r4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def func(c: Int, acc: List[Int], r: RNG): (List[Int], RNG) = {
      if (c == 0) (acc, r)
      else {
        val (i, nr) = int(r)
        func( c - 1, i :: acc, nr)
      }
    }

    func(count, Nil, rng)
  }

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      println("ra " + ra + " rb " + rb)
      val (v1, r2) = ra(rng)
      val (v2, r3) = rb(r2)
      (f(v1, v2), r3)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
    println("fs " + fs)
    fs.foldRight(unit(List[A]()))((f, acc) => {println("f : " + f + " , acc " + acc); map2(f, acc)( _ :: _)})
  }

  def sequence3[A](fs: List[Rand[A]])(rng: RNG) = {
    fs.foldRight(unit(List[A]()))((f, acc) => acc)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {

    rng => {
      def func(list: List[Rand[A]], r: RNG, acc: List[A]) : (List[A], RNG) = {
        list match {
          case Nil => (acc, r)
          case h :: tail => {
            val (v, r1) = h.apply(r)
            func(tail, r1, v :: acc)
          }
        }
      }

      func(fs, rng, Nil)
    }

  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i+(n-1)-mod >= 0) (mod, rng2)
      else nonNegativeLessThan(n)(rng2)
  }

  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt){
      i =>
        val mod = i % n
        if (i+(n-1)-mod >= 0) unit(mod)
        else nonNegativeLessThan2(n)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>{
      val (v1, r2) = f(rng)
      g(v1)(r2)
    }
  }

  def map11[A,B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => unit(f(a)))




  def map21[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        flatMap(ra)(a => map11(rb)(b => f(a, b)))
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}


object Application extends App {

//  println(RNG.int.apply(RNG.Simple(100)))
//  println(RNG.double2(RNG.Simple(100)))
//  println(RNG.ints(4)(RNG.Simple(100)))
//  println(RNG.ints2(4)(RNG.Simple(100)))
//
//  println(RNG.intDouble(RNG.Simple(100)))
//  println(RNG.both(RNG.int, RNG.double)(RNG.Simple(100)))
//
//  println(RNG.both(RNG.int, RNG.int)(RNG.Simple(100)))

  println(RNG.map(RNG.int)(x => 0)(RNG.Simple(100)))

//  println(RNG.map2(RNG.int, RNG.unit(List[Int](1, 2, 3)))(_ :: _)(RNG.Simple(100)))
//  println(RNG.sequence(List(RNG.int, RNG.int))(RNG.Simple(100)))
//  println(RNG.sequence2(List(RNG.int, RNG.int, RNG.int))(RNG.Simple(100)))
//  println(RNG.sequence3(List(RNG.int, RNG.int, RNG.int))(RNG.Simple(100)))
}