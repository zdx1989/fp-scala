package chp1

/**
  * Created by zhoudunxiong on 2017/10/31.
  */
trait List[+A] {

  def header = this match {
    case Nil => sys.error("Empty List")
    case Cons(head, tail) => head
  }

  def tails = this match {
    case Nil => sys.error("Empty List")
    case Cons(head, tail) => tail
  }

  def take(n: Int): List[A] = n match {
    case m if m < 0 => sys.error("n must more than 0")
    case 0 => Nil
    case _ => this match {
      case Nil => Nil
      case Cons(head, tail) => Cons(head, tail.take(n - 1))
    }
  }

  def takeWhile(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(head, tail) => if(f(head)) Cons(head, tail.takeWhile(f)) else Nil
  }

  def drop(n: Int): List[A] = n match {
    case m if m < 0 => sys.error("n must more than 0")
    case 0 => this
    case _ => this match {
      case Nil => Nil
      case Cons(head, tail) => tail.drop(n - 1)
    }
  }

  def dropWhile(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(head, tail) => if(f(head)) tail.dropWhile(f) else this
  }

  def length: Int = this match {
    case Nil => 0
    case Cons(head, tail) => 1 + tail.length
  }

  def init: List[A] = this match {
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, tail.init)
  }

  def ++[B >: A](a: List[B]): List[B] = this match {
    case Nil => a
    case Cons(head, tail) => Cons(head, tail ++ a)
  }

  def map[B](f: A => B): List[B] = this match {
    case Nil => Nil
    case Cons(head, tail) => Cons(f(head), tail.map(f))
  }

  def filter(f: A => Boolean): List[A] = this match {
    case Nil => Nil
    case Cons(head, tail) => if(f(head)) Cons(head, tail.filter(f)) else tail.filter(f)
  }

  def flatMap[B](f: A => List[B]): List[B] = this match {
    case Nil => Nil
    case Cons(head, tail) => f(head) ++ tail.flatMap(f)
  }

}

case class Cons[+A](head: A, tail: List[A]) extends List[A]

case object Nil extends List[Nothing]

object List {

  def apply[A](as: A*): List[A] =
    if(as.isEmpty)
      Nil
    else
      Cons(as.head, apply(as.tail:_*))

  def foldRight[A, B](l: List[A], z: B)(op: (A, B) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => op(head, foldRight(tail, op(head, z))(op))
  }

  def foldLeft[A, B](l: List[A], z: B)(op: (A, B) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, op(head, z))(op)
  }

  def reduceLeft[A](l: List[A])(op: (A, A) => A): A = l match {
    case Nil => sys.error("Empty List!")
    case Cons(head, tail) => foldLeft(tail, head)(op)
  }

  def reduceRight[A](l: List[A])(op: (A, A) => A): A = l match {
    case Nil => sys.error("Empty List!")
    case Cons(head, tail) => op(head, reduceRight(tail)(op))
  }

  //def filter1[A](li: List[A])(f: A => Boolean): List[A] =

}
