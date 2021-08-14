import cats._
import cats.implicits._

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {

  implicit val toStringShow: Show[Account] = Show.fromToString

  implicit def orderById(implicit longOrd: Order[Long]) : Order[Account] =
    Order.from((a1, a2) => longOrd.compare(a1.id, a2.id))

  object Instances {

    implicit val byOwnerAndBalance: Show[Account] = Show.show { acc =>
      s"${acc.owner} - $$${acc.balance}"
    }

    implicit val presentOwner: Show[Account] = Show.show { acc =>
      s"This account belongs to ${acc.owner}."
    }

    implicit val orderByNumber: Order[Account] = Order.by(acc => acc.number)

    implicit val orderByBalance: Order[Account] = Order.by(acc => acc.balance)

  }
}

val acc = Account(1, "123", 255.90, "Guilherme")
import Account.Instances.presentOwner
Show[Account].show(acc)

acc.show
