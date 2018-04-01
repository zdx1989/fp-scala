package chp2

import scala.annotation.tailrec

/**
  * Created by zhoudunxiong on 2018/4/1.
  */
object Chp2 {

  def fib(n: Int): Int = {
    @tailrec
    def loop(prev: Int, cur: Int, n: Int): Int =
      if (n <= 0) prev
      else loop(cur, prev + cur, n - 1)
    loop(0, 1, n)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int, res: Boolean): Boolean =
      if(n <= 0) res
      else loop(n - 1, res && ordered(as(n), as(n - 1)))
    loop(as.length, true)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => {
      b => f(a, b)
    }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
