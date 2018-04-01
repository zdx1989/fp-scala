package chp1

/**
  * Created by zhoudunxiong on 2017/10/31.
  */
trait Tree[+A] {

  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(left, right) => left.size + right.size
  }

  def countLeafs: Int = this match {
    case Leaf(_) => 1
    case Branch(left, right) => left.size + right.size
  }


}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Leaf[A], right: Leaf[A]) extends Tree[A]