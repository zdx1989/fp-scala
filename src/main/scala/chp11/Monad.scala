package chp11

import chp6.State
import chp7.Par
import chp7.Par.Par

import scala.collection.immutable.Stream.Empty


/**
  * Created by zhoudunxiong on 2018/3/15.
  */
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](a: F[A])(f: A => F[B]): F[B]

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma){ma => ma}

  override def map[A, B](a: F[A])(f: (A) => B): F[B] =
    flatMap(a)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa){a =>
      map(fb){b =>
        f(a, b)
      }
    }

  def sequence[A](li: List[F[A]]): F[List[A]] =
    traverse(li){fa => fa}

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldLeft(unit(List[B]())){(b, a) => map2(f(a), b)(_ :: _)}

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    val la = List.fill(n)(ma)
    sequence(la)
  }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def filterM[A](la: List[A])(f: A => F[Boolean]): F[List[A]] = {
    def loop(n: Int, res: F[List[A]]): F[List[A]] = n match {
      case m if m < 0 => res
      case _ =>
        val temp = flatMap(res){li =>
          map(f(la(n))){b =>
            if(b) la(n) :: li
            else li
          }
        }
        loop(n - 1, temp)
    }
    loop(la.length - 1, unit(Nil))
  }


}

object monad {

  val monadOption: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](a: Option[A])(f: (A) => Option[B]): Option[B] = a match {
      case None => None
      case Some(a) => f(a)
    }

    override def unit[A](a: => A): Option[A] = Some(a)
  }

  val monadStream: Monad[Stream] = new Monad[Stream] {
    override def flatMap[A, B](a: Stream[A])(f: (A) => Stream[B]): Stream[B] = a match {
        case Empty => Empty
        case head #:: tail => f(head) ++ flatMap(tail)(f)
      }

    override def unit[A](a: => A): Stream[A] = Stream(a)
  }

  val monadList: Monad[List] = new Monad[List] {
    override def flatMap[A, B](a: List[A])(f: (A) => List[B]): List[B] = a match {
      case Nil => Nil
      case head :: tail => f(head) ++ flatMap(tail)(f)
    }

    override def unit[A](a: => A): List[A] = List(a)
  }

  val monadPar: Monad[Par] = new Monad[Par] {
    override def flatMap[A, B](a: Par[A])(f: (A) => Par[B]): Par[B] =
      es => {
        val aa = a(es)
        f(aa.get())(es)
      }

    override def unit[A](a: => A): Par[A] = Par.unit(a)
  }

  val monadId: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](a: Id[A])(f: (A) => Id[B]): Id[B] =
      a.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type f[X] = State[S, X]}) # f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](a: State[S, A])(f: (A) => State[S, B]): State[S, B] =
      a.flatMap(f)
  }

}

case class Id[A](value: A) {

  def map[B](f: A => B): Id[B] = this match {
    case Id(a) => Id(f(a))
  }

  def flatMap[B](f: A => Id[B]) = this match {
    case Id(a) => f(a)
  }
}