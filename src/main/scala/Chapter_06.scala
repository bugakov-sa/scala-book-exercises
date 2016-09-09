object Chapter_06 extends App {
  def e1: Unit = {
    object Conversions {
      def inchesToCentimeters(inches: Double): Double = {
        inches * 2.54
      }

      def gallonsToLiters(gallons: Double): Double = {
        gallons * 3.78541178
      }

      def milesToKilometers(miles: Double): Double = {
        miles * 1.60934
      }
    }

    println(Conversions.inchesToCentimeters(1))
    println(Conversions.gallonsToLiters(1))
    println(Conversions.milesToKilometers(1))
  }

  def e2: Unit = {

    class UnitConversion(private val factor: Double) {
      def convert(value: Double): Double = {
        value * factor
      }
    }

    object InchesToCentimeters extends UnitConversion(2.54)

    object GallonsToLiters extends UnitConversion(3.78541178)

    object MilesToKilometers extends UnitConversion(1.60934)

    println(InchesToCentimeters.convert(1))
    println(GallonsToLiters.convert(1))
    println(MilesToKilometers.convert(1))
  }

  def e3: Unit = {
    object Origin extends java.awt.Point
  }

  def e4 {
    class Point(val x: Double, val y: Double) {
      override def toString = "(" + x + ";" + y + ")"
    }

    object Point {
      def apply(x: Double, y: Double): Point = new Point(x, y)
    }

    println(Point(3, 4))
  }

  def e5: Unit = {
    println(args.reverse.mkString(" "))
  }

  def e67: Unit = {
    object CardSuit extends Enumeration {
      val SPADES = Value("\u2666")
      val HEARTS = Value("\u2665")
      val DIAMONDS = Value("\u2663")
      val CLUBS = Value("\u2660")
    }

    def isRed(suit: CardSuit.Value): Boolean = {
      suit == CardSuit.SPADES || suit == CardSuit.HEARTS
    }

    for (value <- CardSuit.values) {
      println(value + " " + isRed(value))
    }
  }

  e1
  e2
  e3
  e4
  e5
  e67
}

