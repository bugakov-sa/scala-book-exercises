import scala.collection.mutable.ArrayBuffer

object Chapter_08 extends App {
  def e1: Unit = {
    class BankAccount(initialBalance: Double) {
      private var balance = initialBalance

      def deposit(amount: Double) = {
        balance += amount;
        balance
      }

      def withdraw(amount: Double) = {
        balance -= amount;
        balance
      }
    }

    class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
      override def deposit(amount: Double): Double = super.deposit(amount - 1)

      override def withdraw(amount: Double): Double = super.withdraw(amount + 1)
    }

    val a = new CheckingAccount(100)
    println(a.deposit(10))
    println(a.withdraw(10))
  }

  def e2: Unit = {
    class BankAccount(initialBalance: Double) {
      private var _balance = initialBalance

      def balance = _balance

      def deposit(amount: Double) = {
        _balance += amount;
        _balance
      }

      def withdraw(amount: Double) = {
        _balance -= amount;
        _balance
      }
    }

    class SavingsAccount(
                          initialBalance: Double,
                          private val percent: Double,
                          private val comussion: Double,
                          private val withoutComission: Int
                        ) extends BankAccount(initialBalance) {

      private var n = 0

      def earnMonthlyInterest = {
        super.deposit(balance * percent / 100)
        n = 0
      }

      override def deposit(amount: Double): Double = {
        if (n < withoutComission) {
          n += 1
          super.deposit(amount)
        }
        else super.deposit(amount - comussion)
      }

      override def withdraw(amount: Double): Double = {
        if (n < withoutComission) {
          n += 1
          super.withdraw(amount)
        }
        else super.withdraw(amount + comussion)
      }
    }

    val account = new SavingsAccount(1000, 10, 2, 3)
    println(account.balance)
    account.deposit(100)
    println(account.balance)
    account.withdraw(50)
    println(account.balance)
    account.deposit(100)
    println(account.balance)
    account.withdraw(50)
    println(account.balance)
    account.earnMonthlyInterest
    println(account.balance)
  }

  def e4: Unit = {
    abstract class Item {

      def price: Double

      def description: String
    }

    class SimpleItem(val price: Double, val description: String) extends Item

    class Bundle extends Item {
      private val items: ArrayBuffer[Item] = ArrayBuffer()

      def add(item: Item) = items += item

      override def price = items.map(_.price).sum

      override def description: String = items.map(_.description).mkString("items:{", ",", "}")
    }

    val bundle = new Bundle
    bundle.add(new SimpleItem(12, "item1"))
    bundle.add(new SimpleItem(45, "item2"))
    println(bundle.price)
    println(bundle.description)
  }

  def e5: Unit = {
    class Point(val x: Double, val y: Double) {
      override def toString: String = "(" + x + ";" + y + ")"
    }

    class LabeledPoint(val label: String, x: Double, y: Double) extends Point(x, y) {
      override def toString: String = label + super.toString
    }

    println(new LabeledPoint("BlackThursday", 1929, 230.07))
  }

  def e6: Unit = {
    abstract class Shape {
      def centerPoint: (Double, Double)
    }

    class Rectangle(val x1: Double, val x2: Double, val y1: Double, val y2: Double) extends Shape {
      override def centerPoint: (Double, Double) = ((x1 + x2) / 2, (y1 + y2) / 2)
    }

    class Circle(val x: Double, val y: Double, val r: Double) extends Shape {
      override def centerPoint: (Double, Double) = (x, y)
    }

    println(new Rectangle(1, 8, 2, 10).centerPoint)
    println(new Circle(3, 4, 3).centerPoint)
  }

  def e7: Unit = {
    class Square(x: Int, y: Int, w: Int) extends java.awt.Rectangle(x, y, w, w) {

      def this(w: Int) = this(0, 0, w)

      def this() = this(0)
    }

    println(new Square(2, 3, 4))
    println(new Square(4))
    println(new Square())
  }

  e1
  e2
  e4
  e5
  e6
  e7
}
