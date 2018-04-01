package chp6

/**
  * Created by zhoudunxiong on 2018/3/4.
  */
trait RNG {

  def nextInt: (Int, RNG)
}


case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val newRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, newRNG)
  }
}

object RNG {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng1) = rng.nextInt
    if (i == Int.MaxValue) (0, rng1)
    else (Math.abs(i), rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    if (i == Int.MaxValue) (0.0, rng1)
    else (i.toDouble / Int.MaxValue, rng1)
  }


  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i1, rng1) = rng.nextInt
    val (i2, rng2) = rng1.nextInt
    ((i1, i2.toDouble), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng1) = intDouble(rng)
    ((d, i), rng1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val ((_, d1), rng1) = intDouble(rng)
    val ((_, d2), rng2) = intDouble(rng1)
    val ((_, d3), rng3) = intDouble(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int, res: (List[Int], RNG)): (List[Int], RNG) = count match {
      case 0 => res
      case _ =>
        val (li, rng) = res
        val (i, rng1) = rng.nextInt
        loop(count - 1, (i :: li, rng1))
    }
    loop(count, (Nil, rng))
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng1) = s(rng)
      (f(a), rng1)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double1(rng: RNG): (Double, RNG) =
    map(nonNegativeInt){i =>
      if (i == Int.MaxValue) 0.0 else i.toDouble / Int.MaxValue
    }(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val int: Rand[Int] = _.nextInt

  val doubles: Rand[Double] =
    rng => {
      val (i, rng1) = rng.nextInt
      (i.toDouble, rng1)
    }

  def intDouble1(rng: RNG): ((Int, Double), RNG) =
    both(int, doubles)(rng)

  def doubleInt1(rng: RNG): ((Double, Int), RNG) =
    both(doubles, int)(rng)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  rng => {
    def loop(n: Int, res: (List[A], RNG)): (List[A], RNG) = n match {
      case m if m < 0 => res
      case _ =>
        val (li, rng) = res
        val (a, rng2) = fs(n)(rng)
        loop(n - 1, (a :: li, rng2))
    }
    loop(fs.length - 1, (Nil, rng))
  }

  def ints1(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = ra(rng)
      f(a)(rng1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = ???

  def map1[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))

  def map22[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){a =>
      map(rb){b =>
        f(a, b)
      }
    }

}


case class State[S, +A](run: S => (A, S)) {

  def unit[B >: A](a: B): State[S, B] = State(s => (a, s))

  def map[B](f: A => B): State[S, B] = State{s =>
    val (a, s1) = run(s)
    (f(a), s1)
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State{s =>
    val (a, s1) = run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State{s =>
    val (a, s1) = run(s)
    f(a).run(s1)
  }

}

object State {

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State {s =>
    def loop(n: Int, res: (List[A], S)): (List[A], S) = n match {
      case m if m < 0 => res
      case _ =>
        val (li, s1) = res
        val (a, s2) = fs(n).run(s1)
        loop(n - 1, (a :: li, s2))
    }
    loop(fs.length - 1, (Nil, s))
  }

  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  def ints(n: Int): Rand[List[Int]] = State{rng =>
    def loop(n: Int, res: (List[Int], RNG)): (List[Int], RNG) =
      n match {
        case 0 => res
        case _ =>
          val (li, rng) = res
          val (i, rng1) = rng.nextInt
          loop(n - 1, (i :: li, rng1))
      }
    loop(n, (Nil, rng))
  }

  val ns: Rand[List[Int]] =
    int.flatMap(x =>
      int.flatMap(y =>
        ints(x).map(xs =>
          xs.map(_ % y)
        )
      )
    )

  val ns1: Rand[List[Int]] =
    for {
      x <- int
      y <- int
      xs <- ints(x)
    } yield xs.map(_ % y)
}