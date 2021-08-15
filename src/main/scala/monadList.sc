import cats._
import cats.implicits._

import scala.annotation.tailrec

val listMonad = new Monad[List] {
  override def pure[A](x: A): List[A] = List(x)

  override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
    fa match {
      case Nil => Nil
      case (h :: t) => f(h) ::: flatMap(t)(f)
    }

  override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = ???
}

val lm = List(1,2,3)
listMonad.flatMap(lm)((a) => List(a+1, a+2, a+3, a+4, a+5))
