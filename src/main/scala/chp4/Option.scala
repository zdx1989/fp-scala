package chp4

/**
  * Created by zhoudunxiong on 2018/2/21.
  */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case _ => None
  }
}

case object None extends Option[Nothing]

case class Some[+A](a: A) extends Option[A]

object Option {

  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    oa.flatMap{a =>
      ob.map{b =>
        f(a, b)
      }
    }

  def map2_new[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- oa
      b <- ob
    } yield f(a, b)

  def sequence[A](li: List[Option[A]]): Option[List[A]] = {
    def loop(n: Int, res: Option[List[A]]): Option[List[A]] = n match {
      case -1 => res
      case _ => li(n) match {
        case None => None
        case Some(a) => loop(n - 1, res.map(a :: _))
      }
    }
    loop(li.length - 1, Some(Nil))
  }

  def traverse[A, B](li: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(li.map(f(_)))


}
