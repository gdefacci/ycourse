package yoox.course

object HigherKind {
  
  type EitherR[T] = Either[Throwable, T]
  
  type Endomorphism[T] = T => T
  
  type Id[T] = T
  
  class Const[T]() {
    type Type[X] = T
  }
  
  object Const {
    
    def apply[T] = new Const[T]
    
  }
  
  
}