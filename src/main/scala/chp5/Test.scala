package chp5

/**
  * Created by zhoudunxiong on 2018/2/25.
  */
object Test {

  def main(args: Array[String]): Unit = {
    import Stream._
    val s = Stream(1, 2, 3, 4)
    //val s: Stream[Int] = Empty
    println(s)
    println(s.toList)
    println(s.take(2))
    println(s.take(2).toList)
    println(s.take1(2).toList)
    println(s.drop(2).toList)
    println(s.takeWhile(_ > 2).toList)
    println(s.takeWhile1(_ > 2).toList)
    println(s.takeWhile2(_ > 2).toList)
//    println(s.forAll(_ > 0))
//    println(s.headOption)
//    println(s.headOption1)
//    println(s.map(_ + 1).toList)
//    println(s.map1(_ + 1).toList)
//    println(s.map2(_ + 1).toList)
//    println(s.filter(_ > 1).toList)
//    println(s.filter1(_ > 1).toList)
//    println(s.append(Stream(5, 6)).toList)
//    println(s.append1(Stream(5, 6)).toList)
//    println(s.flatMap(a => Stream(a, a)).toList)
//    println(s.flatMap1(a => Stream(a, a)).toList)
//    println(ones.take(3).toList)
//    println(ones1.take(3).toList)
//    println(constant("zdx").take(3).toList)
//    println(constant1("zdx").take(3).toList)
//    println(from(7).take(5).toList)
//    println(from1(7).take(5).toList)
//    println(fibs.take(8).toList)
//    println(fibs1.take(8).toList)
  }
}
