import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Chapter_03 extends App {

  object Utils {
    def printArray(a: Array[Int]) = {
      for (ai <- a)
        println(ai)
    }
  }

  def e1: Unit = {
    def createSeqArray(n: Int): Array[Int] = {
      val a = new Array[Int](n)
      for (i <- 0 until a.length)
        a(i) = i
      a
    }

    Utils.printArray(createSeqArray(10))
  }

  def e2: Unit = {
    Utils.printArray(swapPairs(Array(1, 2, 3, 4, 5)))

    def swapPairs(array: Array[Int]): Array[Int] = {
      for (i <- 0 until (array.length / 2) * 2 if i % 2 == 0) {
        val temp = array(i)
        array(i) = array(i + 1)
        array(i + 1) = temp
      }
      array
    }
  }

  def e3: Unit = {
    Utils.printArray(swapPairs(Array(1, 2, 3, 4, 5)))

    def swapPairs(array: Array[Int]): Array[Int] = {
      (for (i <- 0 until array.length) yield {
        if (i % 2 == 1) array(i - 1)
        else if (i + 1 < array.length) array(i + 1)
        else array(i)
      }).toArray
    }
  }

  def e4: Unit = {
    Utils.printArray(f(Array(1, -2, 3, -4, 0, 5)))

    def f(array: Array[Int]): Array[Int] = {
      Array.concat(
        for (a <- array if a > 0) yield a,
        for (a <- array if a <= 0) yield a)
    }
  }

  def e5: Unit = {
    val a: Array[Double] = Array(1.2, 4.0, 0.5)
    println(a.sum / a.length)
  }

  def e6: Unit = {
    val a: Array[Int] = Array(34, 7, 2, 556, 23, 0)
    Utils.printArray(a.sortWith(_ >= _))

    val b = new ArrayBuffer[Int]
    for (ai <- a) b += ai
    println(b.sortWith(_ >= _))
  }

  def e7: Unit = {
    val a: Array[Int] = Array(34, 7, 2, 556, 23, 34, 7, 0)
    Utils.printArray(a.distinct)
  }

  def e8: Unit = {
    def f(a: ArrayBuffer[Int]): ArrayBuffer[Int] = {
      val indexes: IndexedSeq[Int] = (for (i <- 0 until a.length if (a(i) < 0)) yield i).reverse
      for (i <- 0 until indexes.length - 1) a.remove(indexes(i))
      a
    }

    val a: ArrayBuffer[Int] = f(ArrayBuffer(1, -2, 3, -4, 0, 5))
    for (ai <- a) println(ai)
  }

  def e9: Unit = {
    val a = java.util.TimeZone.getAvailableIDs
      .filter(_.startsWith("America"))
      .map(_.substring(8))
    for (ai <- a)
      println(ai)
  }

  def e10: Unit = {
    import java.awt.datatransfer._

    import scala.collection.JavaConversions.asScalaBuffer

    val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]
    val res: mutable.Buffer[String] = flavors.getNativesForFlavor(DataFlavor.imageFlavor)
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
