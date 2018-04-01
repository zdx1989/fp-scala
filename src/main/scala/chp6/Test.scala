package chp6

/**
  * Created by zhoudunxiong on 2018/3/4.
  */
object Test {

  def main(args: Array[String]): Unit = {
    import RNG._
    val rng = SimpleRNG(42)
    println(nonNegativeInt(rng))
    println(ints(3)(rng))
    println(double(rng))
    println(double1(rng))
  }
}
