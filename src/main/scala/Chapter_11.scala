import scala.collection.mutable.ArrayBuffer

object Chapter_11 extends App {
  def e3: Unit = {
    class Fraction(n: Int, d: Int) {
      private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)
      private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)

      override def toString: String = num + "/" + den

      def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0

      def gcd(a: Int, b: Int): Int = if (b == 0) math.abs(a) else gcd(b, a % b)

      def +(other: Fraction) = new Fraction(num * other.den + other.num * den, den * other.den)

      def -(other: Fraction) = new Fraction(num * other.den - other.num * den, den * other.den)

      def *(other: Fraction) = new Fraction(num * other.num, den * other.den)

      def /(other: Fraction) = new Fraction(num * other.den, den * other.num)
    }

    println(new Fraction(15, -6))
    val a = new Fraction(1, 5)
    val b = new Fraction(4, 16)
    println(a + b)
    println(a - b)
    println(a * b)
    println(a / b)
  }

  def e4: Unit = {
    class Money(d: Int, c: Int) {
      val dollars = totalCents(d, c) / 100
      val cents = totalCents(d, c) % 100

      override def toString: String = dollars + "." + cents + "$"

      private def totalCents(d: Int, c: Int) = d * 100 + c

      private def totalCents: Int = totalCents(dollars, cents)

      def +(other: Money) = new Money(dollars + other.dollars, cents + other.cents)

      def -(other: Money) = new Money(dollars - other.dollars, cents - other.cents)

      def ==(other: Money) = totalCents == other.totalCents

      def <(other: Money) = totalCents < other.totalCents

      def >(other: Money) = totalCents > other.totalCents
    }

    val sum1 = new Money(1, 75)
    val sum2 = new Money(0, 50)
    val sum3 = new Money(2, 25)
    println(sum1 + "+" + sum2 + "=" + (sum1 + sum2))
    println(sum1 + "-" + sum2 + "=" + (sum1 - sum2))
    println(sum1 + "+" + sum2 + "=" + sum3 + " it is " + (sum1 + sum2 == sum3))
  }

  def e5: Unit = {
    class Table {
      private val cells = ArrayBuffer[ArrayBuffer[String]]()
      cells += ArrayBuffer[String]()

      override def toString: String =
        cells map (_ map ("<td>" + _ + "</td>") mkString("<tr>", "", "</tr>")) mkString("<table>", "", "</table>")

      def |(cell: String) = {
        cells(cells.length - 1) += cell
        this
      }

      def ||(cell: String) = {
        cells += ArrayBuffer[String]()
        this | cell
      }
    }

    println(new Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET")
  }

  def e6: Unit = {
    class ASCIIArt(private val picture: ArrayBuffer[String]) {
      override def toString: String = picture mkString "\n"

      def +(other: ASCIIArt) = new ASCIIArt(picture zip other.picture map (p => p._1 + p._2))

      def -(other: ASCIIArt) = new ASCIIArt(picture ++ other.picture)
    }

    val p1 = new ASCIIArt(ArrayBuffer(
      " /\\_/\\ ",
      "( ' ' )",
      "(  -  )",
      " | | | ",
      "(__|__)"
    ))
    val p2 = new ASCIIArt(ArrayBuffer(
      "   -----  ",
      " / Hello \\",
      "<  Scala |",
      " \\ Coder /",
      "   -----  "
    ))
    println(p1)
    println()
    println(p2)
    println()
    println(p1 + p2)
    println()
    println(p1 - p2)
    println()
  }

  def e7: Unit = {
    class BitSequence(private var seq: Long) {
      override def toString: String = (0 to 63) map (this (_)) mkString ("")

      def apply(i: Int) = (seq & (1 << i)) >> i

      def update(i: Int, v: Int): Unit = {
        seq |= 1 << i
        if (v == 0) seq ^= 1 << i
      }
    }

    val seq = new BitSequence(0)
    println(seq)
    seq(2) = 1
    println(seq)
    seq(5) = 1
    println(seq)
    seq(2) = 0
    println(seq)
  }

  def e8: Unit = {
    class Matrix(val m: Int, val n: Int) {
      private val table = new Array[Int](m * n)

      override def toString = table.grouped(m).map(_.mkString(" ")).mkString("\n")

      def apply(i: Int, j: Int) = table(i * n + j)

      def update(i: Int, j: Int, v: Int) = table(i * n + j) = v

      def +(other: Matrix) = {
        if (n != other.n || m != other.m) throw new RuntimeException
        val res = new Matrix(m, n)
        for (i <- 0 until m; j <- 0 until n) res(i, j) = this (i, j) + other(i, j)
        res
      }

      def *(other: Matrix) = {
        if (n != other.m) throw new RuntimeException
        val res = new Matrix(m, other.n)
        for (i <- 0 until m; j <- 0 until other.n)
          res(i, j) = (for (k <- 0 until n) yield this (i, k) * other(k, j)).sum
        res
      }

      def *(c: Int) = {
        val res = new Matrix(m, n)
        for (i <- 0 until m; j <- 0 until n) res(i, j) = c * this (i, j)
        res
      }
    }

    val a = new Matrix(2, 2)
    a(0, 0) = 1
    a(0, 1) = 2
    a(1, 0) = 3
    a(1, 1) = 4
    val b = new Matrix(2, 2)
    b(0, 0) = 1
    b(0, 1) = 0
    b(1, 0) = 0
    b(1, 1) = 1
    println(a)
    println()
    println(b)
    println()
    println(a + b)
    println()
    println(a * b)
    println()
    println(a * 2)
    println()


  }

  def e9: Unit = {
    object RichFile {
      def unapply(file: String) = Some((
        file.substring(0, file.lastIndexOf("/")),
        file.substring(file.lastIndexOf("/") + 1, file.lastIndexOf(".")),
        file.substring(file.lastIndexOf(".") + 1)
        ))
    }

    val RichFile(path, name, ext) = "/home/cay/readme.txt"
    println(path)
    println(name)
    println(ext)
  }

  def e10: Unit = {
    object RichFile {
      def unapplySeq(file: String): Option[Seq[String]] = {
        val res = file.trim.split("/").filter(!_.isEmpty)
        if (res.isEmpty) None else Some(res)
      }
    }

    def test(file: String) = file match {
      case RichFile(dir1, dir2, file) => {
        println("dir1/dir2/file")
        println(dir1)
        println(dir2)
        println(file)
      }
      case _ => println("unknown case")
    }

    test("/home/cay/readme.txt")
    test("/readme.txt")
  }

  e3
  e4
  e5
  e6
  e7
  e8
  e9
  e10
}
