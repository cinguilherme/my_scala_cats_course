import cats._
import cats.implicits._

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  implicit val universalEq: Eq[Account] = Eq.fromUniversalEquals

  object Instances {

    implicit def byIdEq(implicit eqLong: Eq[Long]): Eq[Account] = Eq.instance[Account]((a,b) => eqLong.eqv(a.id, b.id))

    implicit def eqById2(implicit eqLong: Eq[Long]): Eq[Account] = Eq.by(_.id)

    //exercise, compare two acc by number.
    implicit def eqByNumberX(implicit eqStr: Eq[String]): Eq[Account] =
      Eq.instance[Account]((a1, a2) => eqStr.eqv(a1.number, a2.number))

    implicit def eqByNumber(implicit eqStr: Eq[String]): Eq[Account] = Eq.by(_.number)

  }

}
