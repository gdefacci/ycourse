package yoox.course

import scala.language.higherKinds

trait Traverse[F[_]] {
  
  def traverse[G[_],A,B](fga:F[A])(f: A => G[B])(implicit app:Applicative[G]):G[F[B]]
  
  def sequence[G[_],A](fga:F[G[A]])(implicit app:Applicative[G]):G[F[A]] =
    traverse(fga)( i => i)
  
}

object Traverse {
  
  
  implicit val listTraverse = new Traverse[List] {
    
    def traverse[G[_],A,B](fa:List[A])(f: A => G[B])(implicit app:Applicative[G]):G[List[B]] = {
      val glb:G[List[B]] = app.pure(List.empty[B])
      val glb0:G[List[B]] = fa.foldLeft(glb)((buf:G[List[B]], a:A) => app.map2(buf, f(a))( (a,b) => a :+ b))
      app.map(glb0)(identity)
    }
    
  }
  
  implicit val optionTraverse = new Traverse[Option] {
    
    def traverse[G[_],A,B](fa:Option[A])(f: A => G[B])(implicit app:Applicative[G]):G[Option[B]] = {
      val glb:G[Option[B]] = app.pure(None)
      val glb0:G[Option[B]] = fa.foldLeft(glb)((buf:G[Option[B]], a:A) => app.map2(buf, f(a))( (a:Option[B],b:B) => a.orElse(Some(b)) ))
      app.map(glb0)(identity)
    }
    
  }
  
  def apply[F[_]](implicit t:Traverse[F]) = t
  
}