package chp4

/**
  * Created by zhoudunxiong on 2018/2/21.
  */
object Test {

  import Option._

  def main(args: Array[String]): Unit = {
    println(mean(Seq(1, 2, 3, 4)))
    println(variance(Seq(1, 2, 3, 4)))
    val li = List(Some(1), Some(2), Some(3), None)
    println(sequence(li))
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).map(m =>
      xs.map(x => Math.pow(x-m, 2)).sum / xs.length
    )


}
