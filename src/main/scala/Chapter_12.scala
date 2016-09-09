object Chapter_12 extends App {
  def e1: Unit = {
    def values(fun: (Int => Int), low: Int, high: Int) = (low to high) map (x => (x, fun(x)))

    println(values(x => x * x, -5, 5))
  }

  def e2: Unit = {
    def max(a: Array[Int]) = a reduceLeft (math.max(_, _))

    println(max(Array(34, 2, 9, 0, 45)))
  }

  def e3: Unit = {
    def factorial(n: Int) = (1 to n) reduceLeft (_ * _)

    println(factorial(5))
  }

  def e4: Unit = {
    def factorial(n: Int) = (1 to n).foldLeft(1)(_ * _)

    println(factorial(0))
    println(factorial(5))
  }

  def e5: Unit = {
    def largest(fun: (Int) => Int, inputs: Seq[Int]) = inputs map fun max

    println(largest(x => 10 * x - x * x, 1 to 10))
  }

  def e6: Unit = {
    def largest(fun: (Int) => Int, inputs: Seq[Int]) = inputs reduceLeft ((l, r) => if (fun(l) > fun(r)) l else r)

    println(largest(x => 10 * x - x * x, 1 to 10))
  }

  def e7: Unit = {
    def ajustToPair(fun: (Int, Int) => Int) = (p: (Int, Int)) => fun(p._1, p._2)

    println(ajustToPair(_ * _)((6, 7)))
  }

  def e8: Unit = {
    val a1 = Array("qwe", "qwert", "q")
    val a2 = Array(3, 5, 1)
    println(a1.corresponds(a2)(_.length == _))
  }

  def e9: Unit = {
    def corresponds[A, B](a1: Array[A], a2: Array[B], fun: (A, B) => Boolean) = a1.corresponds(a2)(fun)

    val a1 = Array("qwe", "qwert", "q")
    val a2 = Array(3, 5, 1)
    println(corresponds(a1, a2, (x: String, y: Int) => x.length == y))
  }

  def e10: Unit = {
    def unless(condition: => Boolean)(block: => Unit) = if (!condition) block

    unless(1 > 2) {
      println("1>2")
    }
    unless(3 > 2) {
      println("3>2")
    }
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