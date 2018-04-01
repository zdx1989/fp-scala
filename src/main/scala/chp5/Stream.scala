package chp5

/**
  * Created by zhoudunxiong on 2018/2/24.
  */
sealed trait Stream[+A] {
  import Stream._

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = n match {
    case m if m <= 0 => Empty
    case _ => this match {
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
    }
  }

  def take1(n: Int): Stream[A] =
    unfold((n, this)){
      case (m, _) if m <= 0 => None
      case (_, Empty) => None
      case (a, Cons(h, t)) => Some(h(), (a - 1, t()))
    }

  def drop(n: Int): Stream[A] = n match {
    case m if m <= 0 => this
    case _ => this match {
      case Empty => Empty
      case Cons(h, t) => t().drop(n - 1)
    }
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (f(h())) Cons(h, () => t().takeWhile(f)) else t().takeWhile(f)
  }

  def unCons[B >: A](s: Stream[B]): Option[(B, Stream[B])] = s match {
    case Empty => None
    case Cons(h, t) => Some(h(), t())
  }

  def takeWhile2(f: A => Boolean): Stream[A] =
    unfold(unCons(this)){
      case Some((h, t)) if f(h) => Some(h, unCons(t))
      case _ => None
    }

  def foldRight[B](b: => B)(f: (A, => B) => B): B = this match {
    case Empty => b
    case Cons(h, t) => f(h(), t().foldRight(b)(f))
  }

  def forAll(f: A => Boolean): Boolean = this match {
    case Cons(h, t) => f(h()) && t().forAll(f)
    case _ => true
  }

  def forAll1(f: A => Boolean): Boolean =
    foldRight(true)((a, b) => f(a) && b)

  def takeWhile1(f: A => Boolean): Stream[A] =
    foldRight(empty[A]){(a, b) =>
      if (f(a)) cons(a, b)
      else b
    }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOption1: Option[A] = {
    val init: Option[A] = None
    foldRight(init){(a, _) =>
      Some(a)
    }
  }

  def map[B](f: A => B): Stream[B] = this match {
    case Empty => Empty
    case Cons(h, t) => cons(f(h()), t().map(f))
  }

  def map1[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def map2[B](f: A => B): Stream[B] =
    unfold(this){
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def filter(f: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (f(h())) cons(h(), t().filter(f)) else t().filter(f)
  }

  def filter1(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = this match {
    case Empty => s
    case Cons(h, t) => cons(h(), t().append(s))
  }

  def append1[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B >: A](f: A => Stream[B]): Stream[B] = this match {
    case Empty => Empty
    case Cons(h, t) => f(h()).append(t().flatMap(f))
  }

  def flatMap1[B >: A](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def :+[B >: A](b: B): Stream[B] = this match {
    case Empty => Stream(b)
    case Cons(h, t) => cons(h(), t() :+ b)
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }


  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _ *))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def loop(prev: => Int, curr: => Int): Stream[Int] =
      cons(prev, loop(curr, prev + curr))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  val ones1: Stream[Int] = unfold(1)(Some(1, _))

  def constant1[A](a: A): Stream[A] = unfold(a)(Some(a, _))

  def from1(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s + 1))

  def fibs1: Stream[Int] =
    unfold((0, 1)){
      case (prev, curr) => Some((prev, (curr, prev + curr)))
    }
}
