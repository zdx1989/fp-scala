package chp12

/**
  * Created by zhoudunxiong on 2018/3/31.
  */
object Test {

  def main(args: Array[String]): Unit = {
    import Applicative._
    println(validWebForm("", "1989-11", "18565653302"))

  }
}
