package yoox.course

import scala.language.higherKinds

trait MonadPlus[F[_]] extends Monad[F] with MonoidK[F] {
  
  def filter[A](fa:F[A])(pred:A => Boolean):F[A] = {
    flatMap(fa) { a => if (pred(a)) pure(a) else empty[A] }
  }
  
  implicit def monoid[A] = Monoid[F[A]](empty[A], (a, b) => combine(a, b) )
  
  def unite[G[_],A](fga:F[G[A]])(implicit foldable:Foldable[G]):F[A] = {
    flatMap(fga)( ga => foldable.foldMap(ga)( a => pure(a)) )
  }
  
}

object MonadPlus {
  
  lazy val listMonadPlus = new MonadPlus[List] with Monad.ListMonad with MonoidK.ListMonodiK {}
  
  lazy val optionMonadPlus = new MonadPlus[Option] with Monad.OptionMonad with MonoidK.OptionMonodiK {}
  
  class MonadPlusLaws[F[_]](m:MonadPlus[F]) {

    def leftFilterDistribuite[A,B](f1:A => F[B])(implicit eq:Equal[F[B]]) = {
      eq( m.flatMap( m.empty[A] )(f1), m.empty ) 
    }
    
    def rightFilterDistribuite[A,B](fa:F[A])(implicit eq:Equal[F[A]]) = {
      eq( m.flatMap( fa )(a => m.empty[A] ), m.empty ) 
    }
    
  }
  
}

object SampleMP extends App {
  
  
  val r =  MonadPlus.listMonadPlus.unite(List(Option(1),None,Some(2),Some(3),Some(5), None))
  
  println(r)
  
}