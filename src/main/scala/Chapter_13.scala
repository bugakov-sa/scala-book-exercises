object Ex_1 extends App {

  import collection.mutable._

  def indexes(s: String) =
    s.zipWithIndex.foldLeft(Map[Char, Set[Int]]())(
      (map, pair) => {
        map(pair._1) = map.getOrElse(pair._1, SortedSet[Int]()) + pair._2
        map
      })

  println(indexes("Mississippi"))
}