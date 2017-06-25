package unsuck

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import scalaz.OptionT
import scalaz.std.scalaFuture.futureInstance

object Futures {
  def main(args: Array[String]): Unit = {
    require(args.length == 2, "Usage: Options <x> <y>")

    val xO = Try(args(0).toInt).toOption
    val yO = Try(args(1).toInt).toOption

    val xF = Future(Try(args(0).toInt).toOption.getOrElse(-1234))
    val yF = Future(Try(args(1).toInt).toOption.getOrElse(-1234))

    val xFO = Future(Try(args(0).toInt).toOption)
    val yFO = Future(Try(args(1).toInt).toOption)

    println(s"addOption: ${addOption(xO, yO).getOrElse(-1234)}")
    println(s"addOption2: ${addOption2(xO, yO).getOrElse(-1234)}")

    addFuture(xF, yF).onSuccess { case i => println(s"addFuture: ${i}") }
    addFuture2(xF, yF).onSuccess { case i => println(s"addFuture2: ${i}") }

    addFutureOption(xFO, yFO).onSuccess { case iO => println(s"addFutureOption: ${iO.getOrElse(-1234)}") }
    addFutureOption2(FutureOption(xFO), FutureOption(yFO)).inner.onSuccess { case iO => println(s"addFutureOption2: ${iO.getOrElse(-1234)}") }

    implicit val futureMonad = new Monad[Future] {
      override def flatMap[A, B](fa: Future[A])(f: A => Future[B]) = fa.flatMap(f)
      override def create[A](a: A) = Future.successful(a)
      override def map[A, B](fa: Future[A])(f: A => B) = fa.map(f)
    }
    addAnyMonadOption(AnyMonadOption(xFO), AnyMonadOption(yFO)).inner.onSuccess { case iO => println(s"addAnyMonadOption: ${iO.getOrElse(-1234)}") }
    addOptionT(OptionT(xFO), OptionT(yFO)).run.onSuccess { case iO => println(s"addOptionT: ${iO.getOrElse(-1234)}") }
  }

  def addOption(xO: Option[Int], yO: Option[Int]): Option[Int] = {
    xO.flatMap(x => yO.map(y => x + y))
  }

  def addOption2(xO: Option[Int], yO: Option[Int]): Option[Int] = {
    for {
      x <- xO
      y <- yO
    } yield x + y
  }

  def addFuture(xF: Future[Int], yF: Future[Int]): Future[Int] = {
    xF.flatMap(x => yF.map(y => x + y))
  }

  def addFuture2(xF: Future[Int], yF: Future[Int]): Future[Int] = {
    for {
      x <- xF
      y <- yF
    } yield x + y
  }

  def addFutureOption2(xFO: FutureOption[Int], yFO: FutureOption[Int]): FutureOption[Int] = {
    for {
      x <- xFO
      y <- yFO
    } yield x + y
  }

  def addFutureOption(xFO: Future[Option[Int]], yFO: Future[Option[Int]]): Future[Option[Int]] = {
    xFO.flatMap {
      case None => Future.successful(None)
      case Some(x) => yFO.map {
        case None => None
        case Some(y) => Some(x + y)
      }
    }
  }

  def addAnyMonadOption(xMO: AnyMonadOption[Future, Int], yMO: AnyMonadOption[Future, Int]): AnyMonadOption[Future, Int] = {
    for {
      x <- xMO
      y <- yMO
    } yield x + y
  }

  def addOptionT(xOT: OptionT[Future, Int], yOT: OptionT[Future, Int]): OptionT[Future, Int] = {
    for {
      x <- xOT
      y <- yOT
    } yield x + y
  }
}

case class FutureOption[A](inner: Future[Option[A]]) {
  def map[B](f: A => B): FutureOption[B] = FutureOption(inner.map(_.map(f)))
  def flatMap[B](f: A => FutureOption[B]): FutureOption[B] = FutureOption {
    inner.flatMap {
      case None => Future.successful(None)
      case Some(a) => f(a).inner
    }
  }
}

trait Monad[M[_]] {
  def map[A, B](ma: M[A])(f: A => B): M[B]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def create[A](a: A): M[A]
}

case class AnyMonadOption[M[_], A](inner: M[Option[A]])(implicit m: Monad[M]) {
  def map[B](f: A => B): AnyMonadOption[M, B] = AnyMonadOption(m.map(inner)(_.map(f)))
  def flatMap[B](f: A => AnyMonadOption[M, B]): AnyMonadOption[M, B] = AnyMonadOption {
    m.flatMap(inner) {
      case None => m.create(None)
      case Some(a) => f(a).inner
    }
  }
}
