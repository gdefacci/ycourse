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
  
  class Reader[C] {
    type Type[A] = ReaderMonad[C,A]
  }
  
  object Reader {
    def apply[T] = new Reader[T]
  }
  
  class State[C] {
    type Type[A] = StateMonad[C,A]
  }
  
  object State {
    def apply[T] = new State[T]
  }
  
}