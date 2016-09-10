object Chapter_10 extends App {
  def e4: Unit = {
    trait Logged {
      println("init logged")

      def log(msg: String) {
        println("logged")
      }
    }

    trait ConsoleLogger extends Logged {
      println("init console")

      override def log(msg: String) {
        println("console")
        println(msg)
      }
    }

    trait TimestampLogger extends Logged {
      println("init timestamp")

      override def log(msg: String) {
        println("timestamp")
        super.log(new java.util.Date() + " " + msg)
      }
    }

    trait ShortLogger extends Logged {
      println("init short")
      val maxLength = 15

      override def log(msg: String) {
        println("short")
        super.log(
          if (msg.length <= maxLength) msg
          else msg.substring(0, maxLength)
        )
      }
    }

    trait CryptoLogger extends Logged {
      println("init crypto")
      val key = 3

      override def log(msg: String): Unit = {
        println("crypto")
        super.log(new String(msg.toStream.map(_.toInt + key).map(_.toChar).toArray))
      }
    }

    class AlgoTrader extends ConsoleLogger with TimestampLogger with ShortLogger {

      println("init algo")

      def start: Unit = {
        log("Starting trading")
      }

      def stop: Unit = {
        log("Stopping trading")
      }
    }

    val trader = new AlgoTrader with CryptoLogger
    trader.start
    trader.stop
    val trader2 = new AlgoTrader with CryptoLogger {
      override val key = -3
    }
    trader2.start
    trader2.stop
  }

  def e5: Unit = {
    import java.beans.{PropertyChangeEvent, PropertyChangeListener}
    import java.beans.{PropertyChangeSupport => JavaPropertyChangeSupport}

    trait PropertyChangeSupport {
      val pcs = new JavaPropertyChangeSupport

      def addPropertyChangeListener(listener: PropertyChangeListener) {
        pcs.addPropertyChangeListener(listener);
      }

      def removePropertyChangeListener(listener: PropertyChangeListener) {
        pcs.removePropertyChangeListener(listener);
      }
    }

    val point = new java.awt.Point with PropertyChangeSupport {
      override def setLocation(x: Int, y: Int): Unit = {
        val oldX = getX
        val oldY = getY
        super.setLocation(x, y)
        pcs.firePropertyChange("x", oldX, getX)
        pcs.firePropertyChange("y", oldY, getY)
      }
    }
    point.addPropertyChangeListener(new PropertyChangeListener {
      override def propertyChange(evt: PropertyChangeEvent): Unit = {
        println(evt.getPropertyName + "  " + evt.getOldValue + " => " + evt.getNewValue)
      }
    })
    point.setLocation(1, 2)
    point.setLocation(2, 3)
  }

  def e10: Unit = {
    abstract class IterableInputStream extends java.io.InputStream with Iterable[Byte] {
      override def iterator: Iterator[Byte] = {
        new Iterator[Byte] {
          override def hasNext: Boolean = available > 0

          override def next(): Byte = read.toByte
        }
      }
    }
  }

  e4
  e5
  e10
}