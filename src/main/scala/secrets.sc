import cats.Functor
import cats._
import cats.implicits._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

class Secret[A](val value: A) {
  private def hashed : String = "HASHED - "+ value
  override def toString: String = hashed
}

def validatedName(name: String) =
  if(name.forall(_.isLetter)) Valid(name)
  else Invalid(List("name must contain only letters"))

def validateAge(age: Int): Validated[List[String], Int] =
  if(age > 18) Valid(age)
  else Invalid(List("age must be greater than 18"))

object Secret {
  implicit val secretFunctor = new Functor[Secret] {
    override def map[A,B](fa: Secret[A])(f: A => B): Secret[B] =
      new Secret[B](f(fa.value))
  }
}


val optionFunctor = new Functor[Option] {
  override def map[A,B](o: Option[A])(f: A => B): Option[B] = {
    o match {
      case None => None
      case Some(v) => Some(f(v))
    }
  }
}

val listFunctor = new Functor[List] {
  override def map[A,B](o: List[A])(f: A => B): List[B] = {
    o match {
      case Nil => Nil
      case head :: tail => f(head) :: map(tail)(f)
    }
  }
}

case class Person(name: Secret[String]) {

  def toUpper(name: Secret[String]): Secret[String] =
    new Secret[String](name.value.toUpperCase)

  def toLower(name: Secret[String]): Secret[String] =
    new Secret[String](name.value.toLowerCase)

}

val p = Person(new Secret[String]("Fred"))

println(s"the person created $p")

val se = new Secret[String](value = "Hey")
se.value

val s2 = Functor[Secret].map(se)(_.toUpperCase)
s2.value

val xx = Option("select")
optionFunctor.map(xx)(_.toUpperCase)

val zz = List(1,2,3,4)
listFunctor.map(zz)((x) => x +1)

