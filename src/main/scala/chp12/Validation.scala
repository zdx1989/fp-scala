package chp12

/**
  * Created by zhoudunxiong on 2018/3/31.
  */
sealed trait Validation[+E, +A] {

}

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]
