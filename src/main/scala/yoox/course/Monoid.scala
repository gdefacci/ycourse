package yoox.course

trait Monoid[T] {
  
  def zero:T
  def append(a:T, b:T):T
  
}

object Monoid {
  
  def apply[T](z:T, sum:(T,T) => T) = new Monoid[T] {
    def zero:T = z
    def append(a:T, b:T):T = sum(a,b)
  }
  
  def append[T](a:T, b:T)(implicit m:Monoid[T]) =
    m.append(a, b)
    
  def appendAll[T](vs:T*)(implicit m:Monoid[T]):T = vs.toSeq match {
    case Seq() => m.zero
    case hd +: rest => m.append(hd, appendAll(rest:_*))
  }
  
  implicit val int = Monoid[Int](0,_+_)
  implicit val long = Monoid[Long](0,_+_)
  implicit val string = Monoid[String]("",_ concat _)
  implicit def list[T] = Monoid[List[T]](Nil,_ ++ _)
  
  def listFold[T](implicit m:Monoid[T]) = Monoid[List[T]](Nil, (a,b) => appendAll[T]((a ++ b):_*) :: Nil)

  implicit def option[T](implicit m:Monoid[T]) = 
    Monoid[Option[T]](None, {
      case (None, None) => None
      case (a,b) => Some(m.append(a.getOrElse(m.zero), b.getOrElse(m.zero)))
    } )

  implicit def pair[A,B](implicit ma:Monoid[A], mb:Monoid[B]) = Monoid[(A,B)](ma.zero -> mb.zero, (a,b) => ma.append(a._1, b._1) -> mb.append(a._2, b._2) )
    
  implicit def fun[T] = Monoid[T => T](i => i, (a,b) => a.andThen(b) )
    
  object Laws {
    
    def associative[T](a:T,b:T,c:T)(implicit monoid:Monoid[T], eq:Equal[T]):Boolean =
      eq( monoid.append( monoid.append(a, b), c), monoid.append( a, monoid.append(b, c)))  

    def identity[T](a:T)(implicit monoid:Monoid[T], eq:Equal[T]):Boolean =
      eq( monoid.append(a, monoid.zero), a) && eq( a, monoid.append(a, monoid.zero))   
      
  }  
    
}

