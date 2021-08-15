import cats._
import cats.data.OptionT
import cats.implicits._

case class Person(name: String)
case class Account(balance: Double, owner: Person)
case class Transfer(source: Account, dest: Account, amount: Double)


val p1 = Person("Fred")
val p2 = Person("Flint")

val ac1 = Account(100.00, p1)
val ac2 = Account(500.00, p2)

val t1 = Transfer(ac1, ac2, 20.0)
val t2 = Transfer(ac2, ac1, 10.0)
val t3 = Transfer(ac1, ac2, 60.0)

val listT = List(t1,t2,t3)
val listP = List(p1,p2)
val listAC = List(ac1, ac2)

def findByPersonName(name: String): Option[Person] = listP.find(p => p.name.equalsIgnoreCase(name))
def findAccountByPerson(person: Person): Option[Account] = listAC.find(ac => ac.owner.equals(person))
def findLastTransferByAccount(account: Account): Option[Transfer] = {
  Option(listT.filter(t => t.source.equals(account)).last)
}
def findLastTransferPersonName(name: String): Option[Transfer] = {
  Option(listT.filter(t => t.source.owner.name.equalsIgnoreCase(name)).last)
}



findLastTransferPersonName("Fred")
findLastTransferPersonName("Flint")
