import java.util

import scala.collection.mutable
import scala.io.Source

object Chapter_04 extends App {
  def e1: Unit = {
    val map = Map("pos1" -> 10, "pos2" -> 15)
    val res: Map[String, Double] = for ((k, v) <- map) yield (k, 0.9 * v)
    for ((k, v) <- res) println(k + "  " + v)
  }

  def e2: Unit = {
    val map = new mutable.HashMap[String, Int]()
    for (word <- Source.fromFile("E:\\file1.txt").mkString.split("\\s+")) {
      map(word) = map.getOrElse(word, 0) + 1
    }
    for ((k, v) <- map) println(v + "\t" + k)
  }

  def e3: Unit = {
    var map = Map[String, Int]()
    for (word <- Source.fromFile("E:\\file1.txt").mkString.split("\\s+")) {
      val count = map.getOrElse(word, 0)
      map = map - word + (word -> (count + 1))
    }
    for ((k, v) <- map) println(v + "\t" + k)
  }

  def e4: Unit = {
    var map = scala.collection.immutable.SortedMap[String, Int]()
    for (word <- Source.fromFile("E:\\file1.txt").mkString.split("\\s+")) {
      val count = map.getOrElse(word, 0)
      map = map - word + (word -> (count + 1))
    }
    for ((k, v) <- map) println(v + "\t" + k)
  }

  def e5: Unit = {
    var map = new util.TreeMap[String, Int]()
    for (word <- Source.fromFile("E:\\file1.txt").mkString.split("\\s+")) {
      if (map.containsKey(word)) {
        map.put(word, map.get(word) + 1)
      }
      else {
        map.put(word, 1)
      }
    }

    import scala.collection.JavaConversions.mapAsScalaMap

    for ((k, v) <- map) println(k + "\t" + v)
  }

  def e6: Unit = {
    val map: mutable.LinkedHashMap[String, Int] = new mutable.LinkedHashMap[String, Int]()
    map("Mondey") = java.util.Calendar.MONDAY
    map("Tuesday") = java.util.Calendar.TUESDAY
    map("Wednesday") = java.util.Calendar.WEDNESDAY
    map("Thursday") = java.util.Calendar.THURSDAY
    map("Friday") = java.util.Calendar.FRIDAY
    map("Saturday") = java.util.Calendar.SATURDAY
    map("Sunday") = java.util.Calendar.SUNDAY
    for ((k, v) <- map) println(v + "\t" + k)
  }

  def e7: Unit = {
    import scala.collection.JavaConversions.asScalaSet

    var maxLength = 0
    for (entry <- System.getProperties.entrySet()) {
      maxLength = scala.math.max(maxLength, entry.getKey.toString.length)
    }
    for (entry <- System.getProperties.entrySet()) {
      val k: String = entry.getKey.toString
      println(k.padTo(maxLength, ' ') + "| " + entry.getValue)
    }
  }

  def e8: Unit = {
    println(minmax(Array(1, 5, 2, 67)))

    def minmax(array: Array[Int]): (Int, Int) = {
      (array.min, array.max)
    }
  }

  def e9: Unit = {
    println(lteqgt(Array(3, 7, 4, 0, 6, 2, 4, 7, 3), 3))

    def lteqgt(array: Array[Int], n: Int): (Int, Int, Int) = {
      (
        array.count(_ < n),
        array.count(_ == n),
        array.count(_ > n)
        )
    }
  }

  def e10: Unit = {
    println("Hello".zip("World"))
  }

  e1
  e2
  e3
  e4
  e5
  e6
  e7
  e8
  e9
  e10
}