import cats._
import cats.implicits._

object eitherMonadsApp extends App {

  object MonadEither {

    implicit def either[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {

      override def pure[A](x: A): Either[E, A] = x.asRight[E]

      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = {
        fa match {
          case Right(v) => f(v)
          case Left(x) => Left(x)
        }
      }

      override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]) = ???
    }

  }

  


}
