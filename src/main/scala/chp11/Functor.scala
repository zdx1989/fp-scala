package chp11

/**
  * Created by zhoudunxiong on 2018/3/15.
  */
trait Functor[F[_]] {

  def map[A, B](a: F[A])(f: A => B): F[B]
}

object Functor {


}