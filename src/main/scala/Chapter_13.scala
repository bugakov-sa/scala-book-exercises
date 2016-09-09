object Chapter_13 extends App {
  def e1: Unit = {
    import collection.mutable._

    def indexes(s: String) =
      s.zipWithIndex.foldLeft(Map[Char, Set[Int]]())(
        (map, pair) => {
          map(pair._1) = map.getOrElse(pair._1, SortedSet[Int]()) + pair._2
          map
        })

    println(indexes("Mississippi"))
  }

  def e2: Unit = {
    def indexes(s: String) =
      s.zipWithIndex.foldLeft(Map[Char, List[Int]]())(
        (map, pair) => {
          map + (pair._1 -> (map.getOrElse(pair._1, Nil) :+ pair._2))
        }
      )

    println(indexes("Mississippi"))
  }

  def e4: Unit = {
    def encode(s: Iterable[String], m: Map[String, Int]) = s.flatMap(m get _)

    println(encode(Array("Tom", "Fred", "Harry"),
      Map(("Tom" -> 3), ("Dick" -> 4), ("Harry" -> 5))))
  }

  def e5: Unit = {
    def mkString(collection: Iterable[Any], start: String, delimiter: String, end: String) =
      start + collection.reduceLeft(_ + delimiter + _) + end

    println(mkString((3 to 9), "<", ",", ">"))
  }

  def e6: Unit = {
    val prices = List(5.0, 20.0, 9.95)
    val quantities = List(10, 2, 1)
    println((prices zip quantities) map (Function.tupled(_ * _)) sum)
  }

  e1
  e2
  e4
  e5
  e6
}