object Chapter_18 extends App {
  def e1: Unit = {
    class Bug {
      private var position = 0
      private var direction = 1

      def move(distance: Int) = {
        position += direction * distance
        this
      }

      def turn = {
        direction *= -1
        this
      }

      def show = {
        print(position + " ")
        this
      }
    }

    (new Bug).move(4).show.move(6).show.turn.move(5).show
    println()
  }

  def e2: Unit = {

    object show
    object then
    object around

    class Bug {
      private var position = 0
      private var direction = 1

      def move(distance: Int): this.type = {
        position += direction * distance
        this
      }

      def turn(p: around.type): this.type = {
        direction *= -1
        this
      }

      def and(p: show.type): this.type = {
        print(position + " ")
        this
      }

      def and(p: then.type): this.type = this
    }

    new Bug move 4 and show and then move 6 and show turn around move 5 and show
    println()
  }

  def e3: Unit = {
    sealed abstract class Field
    case object Title extends Field
    case object Author extends Field

    class Document {
      private var useNextArgAs: Any = null
      private var title = ""
      private var author = ""

      def set(obj: Field) = {
        useNextArgAs = obj;
        this
      }

      def to(arg: String) = {
        if (useNextArgAs != null) {
          useNextArgAs match {
            case Title => title = arg
            case Author => author = arg
            case _ =>
          }
        }
        this
      }

      override def toString: String = title + " - " + author
    }

    println(new Document set Title to "Scala for the Impatient" set Author to "Cay Horstmann")
  }

  def e4: Unit = {
    class Network {
      outer =>

      class Member(val name: String) {

        private def outerNetwork = outer

        override def equals(obj: scala.Any): Boolean = {
          if (!obj.isInstanceOf[Member])
            false
          else {
            val other = obj.asInstanceOf[Member]
            if (outerNetwork != other.outerNetwork)
              false
            else
              name.equals(other.name)
          }
        }

        override def toString: String = "Member{" + name + "}"
      }

      def create(name: String) = new Member(name)
    }

    val network1 = new Network
    val network2 = new Network
    val network1Members = Array(
      network1.create("1"),
      network1.create("2"),
      network1.create("1")
    )
    val network2Members = Array(
      network2.create("1"),
      network2.create("2"),
      network2.create("1")
    )
    for (i <- 0 to 2; j <- 0 to 2) println("Network 1 member " + network1Members(i) + " equals network 2 member " +
      network2Members(j) + " " + network1Members(i).equals(network2Members(j)))
    for (i <- 0 to 2; j <- 0 to 2) println("Network 1 member " + network1Members(i) + " equals network 1 member " +
      network1Members(j) + " " + network1Members(i).equals(network1Members(j)))
  }

  def e6: Unit = {
    def f(a: Array[Int], v: Int): Either[String, Int] = {
      if (a.isEmpty)
        Left("array is empty")
      else
        Right(a.indexOf(a.minBy(x => (math.abs(x - v)))))
    }

    println(f(Array(), 4))
    println(f(Array(2, 6, 3), 6))
    println(f(Array(2, 6, 3, 5, 9, 0, 9), 8))
  }

  def e7: Unit = {
    def f(obj: {def close}, handler: => Unit): Unit = {
      try {
        handler
      }
      finally {
        obj.close
      }
    }

    f(new AnyRef {
      def close = println("close")
    }, {
      println("handle")
    })
  }

  def e8: Unit = {
    def printValues(f: {def apply(i: Int): Int}, from: Int, to: Int): Unit = {
      println((for (i <- `from` to `to`) yield f.apply(i)).mkString(" "))
    }

    printValues((x: Int) => x * x, 3, 6)
    printValues(Array(1, 1, 2, 3, 5, 8, 13, 21, 34, 55), 3, 6)
  }

  e1
  e2
  e3
  e4
  e6
  e7
  e8
}
