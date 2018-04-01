package chp10

/**
  * Created by zhoudunxiong on 2018/3/11.
  */
trait Monoid[A] {

  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val intAddition: Monoid[Int] = new Monoid[Int] {

    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {

    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {

    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {

    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {

    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1.andThen(a2)

    override def zero: (A) => A = a => a
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  val stringMonoid: Monoid[String] = new Monoid[String] {

    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  val words = List("He", "ll", "o")
  val s = words.foldLeft(stringMonoid.zero)(stringMonoid.op)

  val nums = List(2, 0, 1, 8)
  val n = nums.foldLeft(intAddition.zero)(intAddition.op)

  def concatenate[A](li: List[A], m: Monoid[A]): A =
    li.foldLeft(m.zero)(m.op)

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = {
        val (aa1, bb1) = a1
        val (aa2, bb2) = a2
        (ma.op(aa1, aa2), mb.op(bb1, bb2))
      }

      override def zero: (A, B) = (ma.zero, mb.zero)
    }

  val m = productMonoid(intAddition, intAddition)
  val p = foldMap(List(1, 2, 3, 4), m)(a => (a, 1))
  val mean = p._1 / p._2
}
