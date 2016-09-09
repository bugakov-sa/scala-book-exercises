import scala.beans.BeanProperty

object Chapter_05 extends App {

  def e1: Unit = {
    class Counter {
      private var value = 0

      def increment() = {
        if (value == Int.MaxValue) {
          value = -1
        }
        value += 1
      }

      def current = value
    }
  }

  def e2: Unit = {
    class BankAccount(private var privateBalance: Int) {

      def balance = privateBalance

      def deposit(money: Int) = privateBalance += money

      def withdraw(money: Int) = privateBalance -= money
    }

    var account = new BankAccount(1000)
    println(account.balance)
    account.deposit(100)
    println(account.balance)
    account.withdraw(200)
    println(account.balance)
  }

  def e3: Unit = {
    class Time(val hours: Int, val minutes: Int) {

      def before(other: Time): Boolean = this.timePerMinutes < other.timePerMinutes

      private def timePerMinutes: Int = hours * 60 + minutes
    }

    val t1 = new Time(15, 40)
    val t2 = new Time(18, 23)
    println(t1.before(t2))
    println(t2.before(t1))
  }

  def e4: Unit = {
    class Time(hours0: Int, minutes0: Int) {

      private val timePerMinutes = hours0 * 60 + minutes0

      def hours = timePerMinutes / 60

      def minutes = timePerMinutes % 60

      def before(other: Time): Boolean = this.timePerMinutes < other.timePerMinutes
    }

    val t1 = new Time(15, 40)
    val t2 = new Time(18, 23)
    println(t1.before(t2))
    println(t2.before(t1))
  }

  def e5: Unit = {
    class Student(@BeanProperty var name: String, @BeanProperty var id: Long) {}

    val student = new Student("Ivanov", 1)
    student.setName("Petrov")
    student.setId(12)
  }

  def e6: Unit = {
    class Person(private var privateAge: Int) {

      if (privateAge < 0) privateAge = 0

      def age = privateAge

      def age_=(newAge: Int) = {
        if (newAge > privateAge) privateAge = newAge
      }
    }

    val p1 = new Person(18)
    println(p1.age)
    val p2 = new Person(-1234)
    println(p2.age)
  }

  def e7: Unit = {
    class Person(name: String) {
      private val split: Array[String] = name.split(" ")
      val firstName = if (split.length > 0) split(0) else ""
      val lastName = if (split.length > 1) split(1) else ""
    }

    val person = new Person("Fred Smith")
    println(person.firstName + " " + person.lastName)
  }

  def e8: Unit = {
    class Car(val producer: String, val model: String, val year: Int = -1, var number: String = "") {}

    new Car("Producer", "model")
    new Car("Producer", "model", 2010)
    new Car("Producer", "model", 2010, "23434")
    new Car("Producer", "model", number = "23434")
  }

  def e10: Unit = {
    class Employee {

      private var _name = "John Q."
      var salary = 0.0

      def this(name: String, salary: Double) {
        this()
        this._name = name
        this.salary = salary
      }

      def name = _name
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
  e10
}
