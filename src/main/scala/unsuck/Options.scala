package unsuck

object Options {
  def main(args: Array[String]): Unit = {
    require(args.length == 2, "Usage: Options <x> <y>")
    val xO = Option[Int](args(0).toInt)
    val yO = Option[Int](args(1).toInt)

    val aO = xO.flatMap(x => yO.map(y => x + y))
    println(aO)

    val bO = for {
      x <- xO
      y <- yO
    } yield x + y
    println(bO)
  }
}
