package chp3

/**
  * Created by zhoudunxiong on 2018/2/16.
  */
object Test {
  import List._
  import Tree._

  def main(args: Array[String]): Unit = {
    println(drop(List(1, 2, 3, 4, 5), -1))

    println(dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 3))

    println(length2(List(1, 2, 3, 4)))

    println(product1(List(1, 2, 3, 4)))

    println(sum1(List(1, 2, 3, 4)))

    println(length1(List(1)))

    println(init(List(1, 2, 3, 4)))

    println(append(List(1, 2), List(3, 4)))

    println(foldRight(List(1, 2, 3, 4), List(5, 6))(Cons(_, _)))

    println(reverse(List(1, 2, 3, 4, 5)))

    println(List(1, 2, 3, 4, 5))

    println(union(List(List(1, 2), List(3, 4))))

    println(List.map(List(1, 2, 3, 4, 5))(_ + 1))

    println(flatMap(List(1, 2, 3))(i => List(i, i)))

    println(flatMap1(List(1, 2, 3))(i => List(i, i)))

    println(zip(List(1, 2, 3), List(4, 5, 6)))

    //println(zip1(List(1, 2, 3), List(4, 5, 6)))

    val t = Branch(Leaf(1), Branch(Leaf(2), Leaf(4)))

    println(size(t))

    println(size1(t))

    println(maximum(t))

    println(maximum1(t))

    println(depth(t))

    println(depth1(t))

    println(Tree.map(t)(_ + "a"))

    println(Tree.map1(t)(_ + "a"))

  }
}
