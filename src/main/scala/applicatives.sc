import cats._
import cats.implicits._

sealed trait Validated[+A]

object Validated {
  case class Valid[+A](a: A) extends Validated[A]
  case class Invalid(erros: List[String]) extends Validated[Nothing]

  implicit val applicative: Applicative[Validated] = new Applicative[Validated] {

    override def pure[A](x: A) = Valid(x)

    override def ap[A, B](ff: Validated[A => B])(fa: Validated[A]) = {
      map2(ff, fa)((f, v) => f(v))
    }

     override def map2[A, B, C](va: Validated[A], vb: Validated[B])(f: (A,B) => C) = {
      (va, vb) match {
        case (Valid(a), Valid(b)) => Valid(f(a,b))
        case (Invalid(a), Valid(b)) => Invalid(a)
        case (Valid(_), Invalid(b)) => Invalid(b)
        case (Invalid(a), Invalid(b)) => Invalid(a ++ b)
      }


    }

  }
}