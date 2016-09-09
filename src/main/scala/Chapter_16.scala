import scala.xml.{Text, Node, Elem}

object Chapter_16 extends App {
  def e1: Unit = {
    val x: Elem = <fred/>
    println(x)
    println(x.label)
    println(x.child)
    val node: Node = <fred/> (0)
    println(node)
    val node1: Node = <fred/> (0)(0)
    println(node1)
  }

  def e2: Unit = {
    val elem: Elem = <ul>
      <li>Opening bracket: [</li>
      <li>Closing bracket: ]</li>
      <li>Opening brace: {{</li>
      <li>Closing brace: }}</li>
    </ul>

    println(elem)
  }

  //@formatter:off
  def e3: Unit = {
    <li>Fred</li> match {
      case <li>{Text(t)}</li> => println("ok")
      case _ => println("error")
    }
    <li>{"Fred"}</li> match {
      case <li>{Text(t)}</li> => println("ok")
      case _ => println("error")
    }
    <li>{Text("Fred")}</li> match {
      case <li>{Text(t)}</li> => println("ok")
      case _ => println("error")
    }
  }
  //@formatter:on

  e1
  e2
  e3
}
