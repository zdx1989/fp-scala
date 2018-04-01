package chp7
import java.awt.Choice
import java.util.concurrent.TimeUnit

/**
  * Created by zhoudunxiong on 2018/3/10.
  */

object Par {

  import java.util.concurrent.{ExecutorService, Future, Callable}

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](a: A) extends Future[A] {
    override def isCancelled: Boolean = false

    override def get(): A = a

    override def get(timeout: Long, unit: TimeUnit): A = a

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true
  }

  //接受一个已求值的A，返回结果将会在另一个线程中执行
  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  //接受一个未求值的A，返回结果将会在另一个线程中执行
  def lazyUnit[A](a: => A): Par[A] = folk(unit(a))

  //从并行计算中抽取结果
  def run[A](s: ExecutorService)(pa: Par[A]): Future[A] = pa(s)

  //将par[A]分配另一个独立的线程中去运行
  def folk[A](pa: => Par[A]): Par[A] = es => {
    es.submit(new Callable[A] {
      override def call(): A = pa(es).get()
    })
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get(), bf.get()))
    }


  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sequence[A](li: List[Par[A]]): Par[List[A]] = {
    def loop(n: Int, res: Par[List[A]]): Par[List[A]] = n match {
      case m if m < 0 => res
      case _ => loop(n - 1, map2(li(n), res)(_ :: _))
    }
    loop(li.length - 1, unit(Nil))
  }

  def parFilter[A](li: List[A])(f: A => Boolean): Par[List[A]] = {
    def loop(n: Int, res: List[Par[A]]): List[Par[A]] = n match {
      case m if m < 0 => res
      case _ =>
        if (f(li(n))) loop(n - 1, lazyUnit(li(n)) :: res)
        else loop(n - 1, res)
    }
    sequence(loop(li.length - 1, Nil))
  }

  def delay[A](pa: => Par[A]): Par[A] = es => pa(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
  es => {
    val i = n(es).get()
    choices(i)(es)
  }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val a = pa(es).get()
      choices(a)(es)
    }
}