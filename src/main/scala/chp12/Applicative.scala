package chp12

import java.util.Date

import chp11.Functor

/**
  * Created by zhoudunxiong on 2018/3/18.
  */
trait Applicative[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  //  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
//    val fabc: F[A => B => C] = unit(f.curried)
//    val fbc = apply(fabc)(fa)
//    apply(fbc)(fb)
//  }


  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(())){(a, _) => f(a)}

  def traverse[A, B](li: List[A])(f: A => F[B]): F[List[B]] =
    li.foldLeft(unit(List[B]())){(b, a) => map2(f(a), b){_ :: _}}

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma){a => a}

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa){(f, a) => f(a)}

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val fabcd = unit(f.curried)
    val fbcd = apply(fabcd)(fa)
    val fcd = apply(fbcd)(fb)
    apply(fcd)(fc)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val fabcde = unit(f.curried)
    val fbcde = apply(fabcde)(fa)
    val fcde = apply(fbcde)(fb)
    val fde = apply(fcde)(fc)
    apply(fde)(fd)
  }
}

object Applicative {


  def apply[F[_]](implicit fa: Applicative[F]): Applicative[F] = fa


  type StringValidation[A] = Validation[String, A]

  implicit def validtionApplicative: Applicative[StringValidation] = new Applicative[StringValidation] {

    override def map2[A, B, C](fa: StringValidation[A],
                               fb: StringValidation[B])(f: (A, B) => C): StringValidation[C] = (fa, fb) match {
      case (Failure(h, t), Success(b)) => Failure(h, t)
      case (Success(a), Failure(h, t)) => Failure(h, t)
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(h1, t1), Failure(h2, t2)) => Failure(h2, t2 ++ Vector(h1) ++ t1)
    }

    override def unit[A](a: => A): StringValidation[A] = Success(a)
  }

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name can not be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } catch {
      case _ => Failure("Birthdate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNum: String): Validation[String, String] =
    if (phoneNum.matches("[0-9]{10}")) Success(phoneNum)
    else Failure("Phone number must be 10 digits")

  case class WebForm(name: String, birthdate: Date, phoneNum: String)

  def validWebForm(name: String, birthdate: String,
                   phoneNum: String)(implicit va: Applicative[StringValidation]): Validation[String, WebForm] =
    va.map3(validName(name), validBirthdate(birthdate), validPhone(phoneNum))(WebForm(_, _, _))


}
