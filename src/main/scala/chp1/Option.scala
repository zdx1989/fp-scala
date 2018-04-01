package chp1

/**
  * Created by zhoudunxiong on 2017/11/1.
  */
trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(value) => Some(f(value))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(value) => f(value)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(value) => if(f(value)) Some(value) else None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(value) => value
  }

  def orElse[B >: A](op: => Option[B]): Option[B] = this match {
    case None => op
    case _ => this
  }
}

case object None extends Option[Nothing]

case class Some[+A](value: A) extends Option[A]


