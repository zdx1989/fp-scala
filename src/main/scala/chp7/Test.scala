package chp7

/**
  * Created by zhoudunxiong on 2018/3/10.
  */
object Test {

  def sum(ints: Seq[Int]): Int =
    ints.foldLeft(0)(_ + _)

  def sum1(ints: IndexedSeq[Int]): Int =
    if (ints.length <= 1)
      ints.headOption.getOrElse(0)
    else {
      val (l, r) = ints.splitAt(ints.length)
      sum1(l) + sum1(r)
    }

  import Par._


  def sum3(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length)
      map2(sum3(l), sum3(r))(_ + _)
    }

  def sum4(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length)
      map2(folk(sum3(l)), folk(sum3(r)))(_ + _)
    }

}
