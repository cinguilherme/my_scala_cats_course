import cats._
import cats.implicits._
import scala.util._

implicit val tryMonad: Monad[Try] = new Monad[Try] {
  override def pure[A](x: A): Try[A] = Success(x)

  override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] =
    fa match {
      case Success(v) => f(v)
      case Failure(x) => Failure(x)
    }

  override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???
}

tryMonad.pure(5).flatMap((a) => Success(a+1))
tryMonad.pure(5).flatMap((a) => Failure(new Exception("Boom")))
tryMonad.pure(5)
  .flatMap((a) => Failure(new Exception("Boom")))
  .flatMap(_ => Failure(new Exception("BOOM 2")))
