package fpinscala.state

case class MyState[S, +A](run: S => (A, S)) {
  def map[B](f:  A => B): MyState[S, B] = {
    MyState( s => {
      val (a1, s1) = run(s)
      (f(a1), s1)
    })
  }

  def map2[B, C](sb: MyState[S, B])(f: (A, B) => C): MyState[S, C] = {
    MyState( s => {
      val (a1, s1) = run(s)
      val (a2, s2) = sb.run(s1)
      (f(a1, a2), s2)
    })
  }

  def flatMap[B](f: A => MyState[S, B]): MyState[S, B] = {
    MyState( s => {
      val (a1, s1) = run(s)
      f(a1).run(s1)
    })
  }

}

object MyState {
  def unit[S, A](a: A) : MyState[S, A] = {
    MyState(s => (a, s))
  }

  def sequences[S, A](sas: List[MyState[S, A]]): MyState[S, List[A]] = {
    def worker(s: S, as: List[MyState[S, A]], acc: List[A]): (List[A], S) = {
      as match {
        case Nil => (acc, s)
        case h :: t => {
          val (a1, s1) = h.run(s)
          worker(s1, t, a1 :: acc)
        }
      }
    }

    MyState( s => worker(s, sas, Nil))
  }

  def get[S]: MyState[S, S] = MyState(s => (s, s))

  def set[S](s: S): MyState[S, Unit] = MyState(_ => ((), s))

  def modify[S](f: S => S): MyState[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield()

  def simulateMachine(input: MyInput): MyState[MyMachine, (Int, Int)] = {
    for {
      _ <- modify((s: MyMachine) => (input, s) match {
        case (_, MyMachine(_, 0, _)) => s
        case (MyCoin, MyMachine(false, _, _)) => s
        case (MyTurn, MyMachine(true, _, _)) => s
        case (MyCoin, MyMachine(true, candy, coin)) =>
          MyMachine(false, candy, coin + 1)
        case (MyTurn, MyMachine(false, candy, coin)) =>
          MyMachine(true, candy - 1, coin)
      })

      s <- get

    } yield(s.candies, s.coins)
  }

}

sealed trait MyInput
case object MyCoin extends MyInput
case object MyTurn extends MyInput

case class MyMachine(locked: Boolean, candies: Int, coins: Int)

object StateApplication extends App {
  println("Hello world")

  println(MyState[Int, Int](s => (1,s)).run(1))
  println( MyState.unit("aaaaa").run(1) )

  println( MyState.sequences[Int, Int](List(MyState.unit(1), MyState.unit(2), MyState.unit(3))).run(1))

  println(MyState.sequences[Int, Int](List(MyState.unit(1), MyState.unit(2), MyState.unit(3))).map(x => 1).run(1))

  println(MyState.sequences[Int, Int](List(MyState.unit(1), MyState.unit(2), MyState.unit(3))).flatMap(x => MyState.unit(22)).run(1))

  println(MyState.get.run(1))
  println(MyState.set(1).run(22))

  println( MyState.modify[Int]((x:Int) => 3).run(100))

  println(MyState.simulateMachine(MyTurn).run(MyMachine(true, 1, 2)) )

}
