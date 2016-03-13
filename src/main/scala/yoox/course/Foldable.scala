package yoox.course

import scala.language.higherKinds

trait Foldable[F[_]] {
  
  def foldMap[A,B](fa:F[A])(f:A => B)(implicit m:Monoid[B]):B = 
    foldLeft[A,B](fa)( m.zero, (acc,itm) => m.append(acc, f(itm))   )
  
  def foldLeft[A,B](fa:F[A])(initial:B, f:(B,A) => B):B 
  
}

object Foldable {
  
  trait ListFoldable extends Foldable[List] {
    def foldLeft[A,B](fa:List[A])(initial:B, f:(B,A) => B):B = fa.foldLeft(initial)(f)
  }

 trait OptionFoldable extends Foldable[Option] {
    def foldLeft[A,B](fa:Option[A])(initial:B, f:(B,A) => B):B = fa.foldLeft(initial)(f)
  }

  implicit val listFoldable = new ListFoldable {}
  implicit val optionFoldable = new OptionFoldable {}
 
}