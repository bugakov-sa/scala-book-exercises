import scala.runtime.Nothing$

/**
  * Created by 1 on 13.09.2016.
  */
object Chapter_17 extends App {
  def e1: Unit = {
    class Pair[T, S](val t: T, val s: S) {
      override def toString: String = "(" + t + ";" + s + ")"

      def swap = new Pair(s, t)
    }

    val pair = new Pair("qwert", 35)
    println(pair)
    println(pair.swap)
  }

  def e2: Unit = {
    class Pair[T](var t1: T, var t2: T) {
      override def toString: String = "(" + t1 + ";" + t2 + ")"

      def swap = {
        val temp = t1
        t1 = t2
        t2 = temp
      }
    }

    val pair = new Pair(23, 45)
    println(pair)
    pair.swap
    println(pair)
  }

  def e3: Unit = {
    class Pair[T, S](val t: T, val s: S) {
      override def toString: String = "(" + t + ";" + s + ")"
    }

    object Pair {
      def swap[T, S](pair: Pair[T, S]) = new Pair(pair.s, pair.t)
    }

    val pair = new Pair(23, "qwert")
    println(pair)
    println(Pair.swap(pair))
  }

  def e6: Unit = {
    def middle[T: Manifest](i: Iterable[T]): Option[T] = {
      val a: Array[T] = i.toArray
      if (a.isEmpty) None
      else Some(a(a.length / 2))
    }

    println(middle(2 :: 5 :: 3 :: 8 :: 4 :: Nil))
  }

  def e10: Unit = {
    class Pair[T, S](var t: T, var s: S) {
      override def toString: String = "(" + t + ";" + s + ")"

      def swap(implicit ev: T =:= S): Unit = {
        val temp = t
        t = s.asInstanceOf[T]
        s = temp.asInstanceOf[S]
      }
    }

    val pair = new Pair(243, 54)
    println(pair)
    pair.swap
    println(pair)
  }

  e1
  e2
  e3
  e6
  e10
}
