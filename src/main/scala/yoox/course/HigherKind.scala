package yoox.course

object HigherKind {
  
  type EitherR[T] = Either[Throwable, T]
  
  type Endomorphism[T] = T => T
  
  
  
}