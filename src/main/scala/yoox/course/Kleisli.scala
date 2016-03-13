package yoox.course

import scala.language.higherKinds

case class Kleisli[F[_],A,B](val run:A => F[B])(implicit val monad:Monad[F]) { self =>
  
  def andThen[C](k: Kleisli[F, B, C]): Kleisli[F, A, C] =  Kleisli((a: A) => monad.flatMap(run(a))(k.run))  
    
  def map[C](f: B => C): Kleisli[F, A, C] =
    Kleisli( a => monad.map(run(a))(f) )
    
  def flatMap[C](f: B => Kleisli[F, A, C]): Kleisli[F, A, C] =
    Kleisli((r: A) => monad.flatMap[B, C](run(r))(((b: B) => f(b).run(r))))
  
  def local[A0](extractFrom: A0 => A): Kleisli[F, A0, B] =
    Kleisli[F, A0, B](extractFrom andThen self.run)
  
//  def traverse[G[_], T](l:G[F[T]])(implicit gm:Monad[G]):F[G[T]] = {
//    
//  }  
  
}

object Kleisli {

  /*
   def traverseList[F[_],A,B](l:List[Kleisli[F,A,B]])(implicit monad:Monad[F]):Kleisli[F,A,List[B]] = Kleisli { a =>
     l.foldLeft(monad.pure(Nil:List[B])) { (acc,itm) => 
      val fb = itm.run(a)  
      monad.flatMap(fb)(b => monad.map(acc)(lst => lst :+ b) )
    }
  }
  */
  
  /*
  def traverse[G[_], T](l:G[Kleisli[F,A,B]])(m:Monoid[G[B]]):Kleisli[F,A,G[B]] = Kleisli { a =>
    l.foldLeft(monad.pure(m.zero)) { (acc,itm) => 
      val fb = itm.run(a)  
      monad.flatMap(fb)(b => monad.map(acc)(lst => m.append(lst, b)) )
    }
  }
  */
 
  def const[F[_],A,B](v:B)(implicit monad:Monad[F]) = 
    Kleisli[F,A,B](a => monad.pure(v))
  
  type Reader[E, A] = Kleisli[HigherKind.Id, E, A]
  
  object Reader {
    def apply[E, A](f: E => A): Reader[E, A] = new Kleisli[HigherKind.Id, E, A](f)
  }
  
}
