import java.io.IOException

import scala.annotation.varargs
import scala.io.Source

object Chapter_15 extends App {
  def e2: Unit = {
    @deprecated class Ex2 @deprecated()(@deprecated val n: Int) {
      @deprecated def f(@deprecated x: Int): Unit = {
        @deprecated val x: String = null: @deprecated
      }
    }
  }

  @varargs def e4(args: Int*) = args.sum

  @throws[IOException] def e5(file: String) = Source.fromFile(file).mkString

  e2
}
