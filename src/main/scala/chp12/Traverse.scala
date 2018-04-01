package chp12

import chp11.Functor

/**
  * Created by zhoudunxiong on 2018/4/1.
  */
trait Traverse[F[_]] extends Functor[F] {

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit ga: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_], A](fga: F[G[A]])(implicit ga: Applicative[G]): G[F[A]] =
    traverse(fga)(a => a)

}

object Traverse {

  def apply[F[_]](implicit tf: Traverse[F]): Traverse[F] = tf

  implicit val listTraverse: Traverse[List] = new Traverse[List] {
    override def map[A, B](a: List[A])(f: (A) => B): List[B] = a match {
      case Nil => Nil
      case head :: tail => f(head) :: map(tail)(f)
    }
  }

  implicit val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def map[A, B](a: Option[A])(f: (A) => B): Option[B] = a match {
      case None => None
      case Some(a1) => Some(f(a1))
    }
  }

  implicit val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def map[A, B](a: Tree[A])(f: (A) => B): Tree[B] =
      Tree(f(a.head), a.tail.map(t => map(t)(f)))
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])
