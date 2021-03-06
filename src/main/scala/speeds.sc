import cats._
import cats.implicits._

case class Speeds(metersPerSecond: Double) {
  def kmPerSecond: Double = metersPerSecond / 1000.0
  def milesPerSecond: Double = metersPerSecond / 1609.34
}

object Speeds {
  def addSpeeds(s1: Speeds, s2: Speeds) : Speeds =
    Speeds(s1.metersPerSecond + s2.metersPerSecond)

  implicit val monoidSpeed: Monoid[Speeds] = Monoid.instance(Speeds(0), addSpeeds)
}


val speed1 = Speeds(20)
val speed2 = Speeds(20)

Monoid[Speeds].combine(speed1, speed2)
speed1 |+| speed2 |+| Speeds.monoidSpeed.empty

Monoid[Speeds].combineAll(List(speed1, speed2, Speeds(600)))
List(speed1, speed2, Speeds(600)).combineAll

val sumMonoid: Monoid[Int] = Monoid.instance(0, (a,b) =>  a+b)
val minMonoid: Monoid[Int] = Monoid.instance(Int.MaxValue, (a,b) => a min b)
val maxMonoid: Monoid[Int] = Monoid.instance(Int.MinValue, (a,b) => a max b)
val strMonoid: Monoid[String] = Monoid.instance("", _ + _)
def listMonoid[A]: Monoid[List[A]] =  Monoid.instance(Nil, _ ++ _)

val str1 = "aaa"
val str2 = "bbb"

Monoid[String].combine(str1, str2)

1 |+| 2
sumMonoid.combine(1,1)
minMonoid.combine(4,6)

listMonoid.combine(List(1,2,3), List(5,6,7))

List(1,2,3) |+| List(5,6,7)

