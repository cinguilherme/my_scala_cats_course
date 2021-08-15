import cats._
import cats.implicits._

sealed trait Validated[+A]

object Validated {
  case class Valid[+A](a: A) extends Validated[A]
  case class Invalid(erros: List[String]) extends Validated[Nothing]

  implicit val applicative: Applicative[Validated] = new Applicative[Validated] {

    override def pure[A](x: A) = Valid(x)

    override def ap[A, B](ff: Validated[A => B])(fa: Validated[A]) = {
      (ff,fa) match {
        case (Valid(a), Valid(b)) => Valid(a(b))
        case (Invalid(a), Valid(b)) => Invalid(a)
        case (Valid(a), Invalid(b)) => Invalid(b)
        case (Invalid(a), Invalid(b)) => Invalid(List(a.toString(),b.toString()))
      }
    }

     override def map2[A, B, C](va: Validated[A], vb: Validated[B])(f: (A,B) => C) = {
       ap(ap(pure(f.curried))(va))(vb)
    }

    def tuple[A, B](va : Validated[A], vb: Validated[B]): Validated[(A,B)] = {
      map2(va,vb)((a,b) => (a,b))
    }

  }
}

val a: Validated[Int] = Applicative[Validated].pure(1)
val b: Validated[Int] = Applicative[Validated].pure(2)
val c: Validated[Int] = Applicative[Validated].pure(3)

(a,b,c).mapN((z,x,y) => z + x + y)

(a,b).mapN((z,x) => z + x)


val optionApp = new Applicative[Option] {
  override def pure[A](x: A) = Some(x)

  override def ap[A, B](ff: Option[A => B])(fa: Option[A]) = {
    (ff,fa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case _ => None
    }

  }
}

val x = Some(4)
val z = Applicative[Option].pure(2)
val y = Applicative[Option].pure(4)

(z,y,x).mapN((q,w,e) => q + w + e)

val lisApp = new Applicative[List] {
  override def pure[A](x: A) = List(x)

  override def ap[A, B](ff: List[A => B])(fa: List[A]) = {

    (ff, fa) match {
      case (f :: fs, s :: ss) => (s :: ss).fmap(f) ++ ap(fs)(s :: ss)
      case _ => Nil
    }

  }
}


lisApp.map2(List(1,2,3), List(6,7))(_ + _)

lisApp.map2[Int, Int, Int](List(1,2,3), List())(_ + _)
