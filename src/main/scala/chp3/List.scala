package chp3

/**
  * Created by zhoudunxiong on 2018/2/16.
  */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(li: List[Int]): Int = li match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def tail[A](li: List[A]): List[A] = li match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def setHead[A](li: List[A], a: A): List[A] = li match {
    case Nil => Nil
    case Cons(h, t) => Cons(a, t)
  }

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def drop[A](li: List[A], n: Int): List[A] = n match {
    case m if m <= 0 => li
    case _ => li match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](li: List[A], f: A => Boolean): List[A] = li match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(t, f)
      else Cons(h, dropWhile(t, f))
  }

  def length[A](li: List[A]): Int = {
    def loop(li: List[A], acc: Int): Int = li match {
      case Nil => acc
      case Cons(_, t) => loop(t, acc + 1)
    }
    loop(li, 0)
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }

  def init[A](li: List[A]): List[A] = {
    def loop(n: Int, li: List[A], res: List[A]): List[A] = n match {
      case m if m <= 1 => res
      case _ => li match {
        case Nil => Nil
        case Cons(h, t) => loop(n - 1, t, append(res, List(h)))
      }
    }
    loop(length(li), li, Nil)
  }

  def foldRight[A, B](li: List[A], b: B)(f: (A, B) => B): B = li match {
    case Nil => b
    case Cons(h, t) => f(h, foldRight(t, b)(f))
  }

  def product(li: List[Double]): Double =
    foldRight(li, 1.0)(_ * _)

  def length1[A](li: List[A]): Int =
    foldRight(li, 0){(_, b) =>
      b + 1
    }

  def foldLeft[A, B](li: List[A], b: B)(f: (B, A) => B): B = li match {
    case Nil => b
    case Cons(h, t) => foldLeft(t, f(b, h))(f)
  }

  def sum1(li: List[Int]): Int =
    foldLeft(li, 0)(_ + _)

  def product1(li: List[Double]): Double =
    foldLeft(li, 1.0)(_ * _)

  def length2[A](li: List[A]) =
    foldLeft(li, 0){(b, _) =>
      b + 1
    }

  def reverse[A](li: List[A]): List[A] =
    foldLeft(li, Nil: List[A]){(b, a) =>
      Cons(a, b)
    }

  def foldLeft1[A, B](li: List[A], b: B)(f: (B, A) => B): B =
    foldRight(li, b){(a, b) =>
      f(b, a)
    }

  def append1[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(l2, l1){(b, a) =>
      Cons(a, b)
    }

  def union[A](li: List[List[A]]): List[A] =
    foldLeft(li, Nil: List[A]){(b, a) =>
      append(b, a)
    }

  def ++(li: List[Int]): List[Int] = li match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, ++(t))
  }

  def toString(li: List[Double]): List[String] =
    foldLeft(li, Nil: List[String]){(b, a) =>
      Cons(a.toString, b)
    }

  def map[A, B](li: List[A])(f: A => B): List[B] = li match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def map1[A, B](li: List[A])(f: A => B): List[B] =
    foldLeft(li, Nil: List[B]){(b, a) =>
      Cons(f(a), b)
    }

  def filter[A](li: List[A])(f: A => Boolean): List[A] = li match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) Cons(h, filter(t)(f))
      else filter(t)(f)
  }

  def filter1[A](li: List[A])(f: A => Boolean): List[A] =
    foldLeft(li, Nil: List[A]){(b, a) =>
      if (f(a)) Cons(a, b)
      else b
    }

  def flatMap[A, B](li: List[A])(f: A => List[B]): List[B] = li match {
    case Nil => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  def flatMap1[A, B](li: List[A])(f: A => List[B]): List[B] =
    foldLeft(li, Nil: List[B]){(b, a) =>
      append(b, f(a))
    }

  def filter2[A](li: List[A])(f: A => Boolean): List[A] =
  flatMap(li){a =>
    if (f(a)) List(a)
    else Nil
  }

  def zip(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (a, Nil) => a
    case (Nil, b) => b
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zip(t1, t2))
  }


  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
    case (a, Nil) => a
    case (Nil, b) => b
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
}


