import scala.concurrent.Promise
import scala.concurrent.ExecutionContext.Implicits.global

val p = Promise[Int]()
p.success(1)

val f = p.future

val x = f.map(i => i)

println(x)
