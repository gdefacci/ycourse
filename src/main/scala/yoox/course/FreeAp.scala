package yoox.course

import scala.language.higherKinds

trait NaturalTransformation[F[_], G[_]] {
  def apply[T](i: F[T]): G[T]
}

sealed trait FreeAp[F[_], A] {

  def foldMap[G[_]](f: NaturalTransformation[F, G])(implicit app:Applicative[G]): G[A]

  def ap[B](f: FreeAp[F, A => B]): FreeAp[F, B] = f match {
    case Pure(g) => map(g)
    case x @ Ap(value, fun) => Ap(value, ap(fun.map(g => (a: A) => (b: x.Input) => g(b)(a))))
  }
  
  def map[B](f: A => B): FreeAp[F, B]

}

object FreeAp {

  implicit def freeInstance[F[_]]: Applicative[({ type Type[T] = FreeAp[F, T] })#Type] =
    new Applicative[({ type Type[T] = FreeAp[F, T] })#Type] {
      def pure[A](a: A) = FreeAp.pure(a)
      def apply[A, B](fa: FreeAp[F, A])(ff: FreeAp[F, A => B]): FreeAp[F, B] = fa.ap(ff)
    }

  def pure[F[_], A](a: A): FreeAp[F, A] = Pure(a)

  def lift[F[_], A](fa: F[A]): FreeAp[F, A] = Ap(fa, Pure((a: A) => a))

}

final case class Pure[F[_], A](value: A) extends FreeAp[F, A] {
  def foldMap[G[_]](f: NaturalTransformation[F, G])(implicit ap: Applicative[G]): G[A] = {
    ap.pure(value)
  }

  def map[B](f: A => B): FreeAp[F, B] = Pure(f(value))

}
final case class Ap[F[_], I, A](value: F[I], function: FreeAp[F, I => A]) extends FreeAp[F, A] {
  type Input = I

  def foldMap[G[_]](f: NaturalTransformation[F, G])(implicit app: Applicative[G]): G[A] = {
    app.apply(f(value))(function.foldMap(f))
  }
  
  def map[B](f: A => B): FreeAp[F, B] = Ap(value, function.map(_ andThen f))
}

