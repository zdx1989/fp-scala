package chp2

import scala.annotation.tailrec

/**
  * Created by zhoudunxiong on 2017/11/2.
  */
trait Stream[+A] {

  def uncons: Option[(A, Stream[A])]

  def empty: Stream[A] = new Stream[A] {
    override def uncons: Option[(A, Stream[A])] = None
  }

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = {
      s.uncons match {
        case None => acc
        case Some((h, t)) =>  go(t, h :: acc)
      }
    }
    go(this, Nil).reverse
  }

}

object Stream {

  def empty[A]: Stream[A] = new Stream[A] {
    override def uncons: Option[(A, Stream[A])] = None
  }

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = new Stream[A] {
    override def uncons: Option[(A, Stream[A])] = Some((h, t))
  }

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))




}
