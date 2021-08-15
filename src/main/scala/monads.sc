import cats._
import cats.implicits._

sealed trait MOption[+A]

object MOption {
  case class MSome[+A](a: A) extends MOption[A]
  case object MNone extends MOption[Nothing]

  implicit val monad: Monad[MOption] = new Monad[MOption] {
    override def pure[A](x: A): MOption[A] = MSome(x)

    override def flatMap[A, B](fa: MOption[A])(f: A => MOption[B]): MOption[B] = {
      fa match {
        case MSome(a) => f(a)
        case MNone => MNone
      }
    }

    override def map[A,B](fa: MOption[A])(f: A => B): MOption[B] =
      flatMap(fa)(a => pure(f(a)))

    override def flatten[A](ffa: MOption[MOption[A]]): MOption[A] =
      flatMap(ffa)(identity)


    override def tailRecM[A, B](a: A)(f: A => MOption[Either[A, B]]): MOption[B] = {
      ???
    }
  }
}

import MOption._

val a: MOption[Int] = Monad[MOption].pure(5)
val b: MOption[Int] = MNone.flatMap((i: Int) => Monad[MOption].pure(i + 1))

val z: MOption[Int] =
  for {
    a <- Monad[MOption].pure(5)
    b <- Monad[MOption].pure(5)
  } yield a + b


