package unsuck

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

object Futures {
  def main(args: Array[String]): Unit = {
    require(args.length == 2, "Usage: Options <x> <y>")
    val xP = Promise[Int]()
    val yP = Promise[Int]()

    xP.success(args(0).toInt)
    yP.success(args(1).toInt)

    val xF = xP.future
    val yF = yP.future

    val a = xF.flatMap(x => yF.map(y => x + y))
    println(a)

    val b = for {
      x <- xF
      y <- yF
    } yield x + y
    println(b)
  }
}
