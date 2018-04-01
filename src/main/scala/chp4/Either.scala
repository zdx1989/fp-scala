package chp4

/**
  * Created by zhoudunxiong on 2018/2/22.
  */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case _ => this
  }

  def map2[EE >: E, B, C](eb: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b <- eb
    } yield f(a, b)
}

case class Left[+E](e: E) extends Either[E, Nothing]

case class Right[+A](a: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](li: List[Either[E, A]]): Either[E, List[A]] = {
    def loop(n: Int, res: Either[E, List[A]]): Either[E, List[A]] = n match {
      case -1 => res
      case _ => li(n) match {
        case Left(e) => Left(e)
        case Right(a) => loop(n - 1, res.map(a :: _))
      }
    }
    loop(li.length - 1, Right(Nil))
  }

  def traverse[E, A, B](li: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    sequence(li.map(f))
}

