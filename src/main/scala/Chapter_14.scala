object Chapter_14 extends App {
  def e2: Unit = {
    def swap(p: (Int, Int)) = p match {
      case (n, m) => (m, n)
    }

    println(swap((2, 5)))
  }

  def e3: Unit = {
    def swap(a: Array[Any]) = {
      a match {
        case Array(a0, a1, _*) => {
          a(0) = a1
          a(1) = a0
        }
        case _ =>
      }
      a
    }

    def print(a: Array[Any]) = println(a.mkString(" "))

    print(swap(Array()))
    print(swap(Array(1)))
    print(swap(Array(1, 2)))
    print(swap(Array(1, 2, 3)))
  }

  def e4: Unit = {
    abstract class Item
    case class Article(description: String, price: Double) extends Item
    case class Bundle(description: String, discount: Double, items: Item*) extends Item
    case class Multiple(count: Int, items: Item*) extends Item

    def price(it: Item): Double = it match {
      case Article(_, p) => p
      case Bundle(_, disc, its@_*) => its.map(price).sum - disc
      case Multiple(cnt, item) => cnt * price(item)
    }

    println(price(Bundle("", 5, Article("", 10), Multiple(2, Article("", 8)))))
  }

  def e5: Unit = {
    def leafSum(tree: List[Any]): Int = tree match {
      case Nil => 0
      case head :: tail => (head match {
        case n: Int => n
        case list: List[_] => leafSum(list)
        case _ => 0
      }) + leafSum(tail)
    }

    println(leafSum(List(List(3, 8), 2, List(5))))
  }

  def e6: Unit = {
    sealed abstract class BinaryTree
    case class Leaf(v: Int) extends BinaryTree
    case class Node(l: BinaryTree, r: BinaryTree) extends BinaryTree

    def sum(tree: BinaryTree): Int = tree match {
      case Leaf(v) => v
      case Node(l, r) => sum(l) + sum(r)
    }

    println(sum(Node(Node(Leaf(3), Leaf(8)), Leaf(5))))
  }

  def e7: Unit = {
    sealed abstract class Tree
    case class Leaf(v: Int) extends Tree
    case class Node(children: Tree*) extends Tree

    def sum(tree: Tree): Int = tree match {
      case Leaf(v) => v
      case Node(children@_*) => children.foldLeft(0)((x, y) => x + sum(y))
    }

    println(sum(Node(Node(Leaf(3), Leaf(8)), Leaf(2), Node(Leaf(5)))))
  }

  def e8: Unit = {
    sealed abstract class Tree
    case class Leaf(v: Int) extends Tree
    case class NodeAgg(fun: (Int, Int) => Int, ch1: Tree, ch2: Tree, ch: Tree*) extends Tree
    case class NodeOne(fun: (Int) => Int, ch: Tree) extends Tree

    def sum(tree: Tree): Int = tree match {
      case Leaf(v) => v
      case NodeOne(op, ch) => op(sum(ch))
      case NodeAgg(op, ch1, ch2, ch@_*) => ch.foldLeft(op(sum(ch1), sum(ch2)))((x, y) => op(x, sum(y)))
    }

    println(sum(NodeAgg(
      (x: Int, y: Int) => x + y,
      NodeAgg(
        _ * _,
        Leaf(3),
        Leaf(8)),
      Leaf(2),
      NodeOne(
        -_,
        Leaf(5)))))
  }

  def e9: Unit = {
    def sum(list: List[Option[Int]]) = list.map(_.getOrElse(0)).sum

    println(sum(None :: Some(2) :: Some(4) :: None :: Nil))
  }

  def e10: Unit = {
    def f(x: Double) = if (x >= 0) Some(math.sqrt(x)) else None
    def g(x: Double) = if (x != 1) Some(1 / (x - 1)) else None
    def compose(f: Double => Option[Double], g: Double => Option[Double]): Double => Option[Double] =
      x => g(x) match {
        case None => None
        case Some(g_res) => f(g_res)
      }
    val h = compose(f, g)
    println("h(2)=" + h(2))
    println("h(1)=" + h(1))
    println("h(0)=" + h(0))
  }

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
