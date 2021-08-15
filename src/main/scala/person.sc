import cats._
import cats.implicits._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

case class Person(name: String, age: Int)


def validatedName(name: String) =
  if(name.forall(_.isLetter)) Valid(name)
  else Invalid(List("name must contain only letters"))

def validateAge(age: Int): Validated[List[String], Int] =
  if(age > 18) Valid(age)
  else Invalid(List("age must be greater than 18"))


def validatedPerson(p: Person): Validated[List[String], Person] = {
  val vname = validateAge(p.age)
  val vage = validatedName(p.name)
  if(vname.isValid && vage.isValid) Valid(p)
  else Invalid(List("person not validated"))
}
val p = Person("abc", 20)
val pi = Person("t560", 17)

validatedPerson(p)
validatedPerson(pi)