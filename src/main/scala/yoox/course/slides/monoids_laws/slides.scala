













/**
 Last time we defined monoid trait
*/

trait Monoid[T] {
  
  def zero:T
  def append(a:T, b:T):T
  
}



















/**
 we defined a companion object
*/

object Monoid {

/**
 with a factory method
*/

  def apply[T](z:T, sum:(T,T) => T) = new Monoid[T] {
    def zero:T = z
    def append(a:T, b:T):T = sum(a,b)
  }

/**
 a method append
*/

  def append[T](a:T, b:T)(implicit m:Monoid[T]) = m.append(a, b)

/**
 and a method appendAll
*/

  def appendAll[T](vs:T*)(implicit m:Monoid[T]):T = vs.toSeq match {
    case Seq() => m.zero
    case hd +: rest => m.append(hd, appendAll(rest:_*))
  }
}













/**
 and a few (implicit) monoid instances
*/


object FewMonoidInstances {

  implicit val int = Monoid[Int](0,_+_)
  implicit val long = Monoid[Long](0,_+_)
  implicit val string = Monoid[String]("",_ concat _)
  implicit def list[T] = Monoid[List[T]](Nil,_ ++ _)
  implicit def option[T](implicit m:Monoid[T]) =
    Monoid[Option[T]](None, {
      case (None, None) => None
      case (a,b) => Some(m.append(a.getOrElse(m.zero), b.getOrElse(m.zero)))
    } )

  implicit def pair[A,B](implicit ma:Monoid[A], mb:Monoid[B]) = Monoid[(A,B)](ma.zero -> mb.zero, (a,b) => ma.append(a._1, b._1) -> mb.append(a._2, b._2) )
    
  implicit def fun[T] = Monoid[T => T](i => i, (a,b) => a.andThen(b) )
  
}






















/**
 then we saw few usages:
*/
object Sample1 {

  import FewMonoidInstances._
  
  Monoid.appendAll(Some(12), None, Some(4))
  Monoid.appendAll(Some(List(1233, 3443)), None, Some(List(343)))
  Monoid.appendAll(Some((i:Int) => i * 3), None, Some((i:Int) => i * 4))
  

}




























/**
 then we saw few usages:
*/
object Sample2 {

  import FewMonoidInstances._
  // Monoid.appendAll(Some(true), None, Some(true))  does   could not find implicit value for parameter m: Monoid[Option[Boolean]]

}































/**
We also said tha monoids obeys two laws:

associativity:  (a + b) + c == a + (b + c)
identity     :  (a + zero) == a
                (zero + a) == a
*/
































/**
We also said tha monoids obeys two laws:

associativity:  append( append(a,b), c ) == append( a, append(b,c) )
identity     :  append( zero, a ) == a  and
                append( a, zero ) == a
*/






























/**
Today wi will try to see if those laws holds for some of the monoid we defined.

Steps:
  introduce an Equal trait
  defined class Monoid.Laws
  introduce scala check
  use scalacheck to prove (sort of) laws hold
  
*/





























/**
 demo Equal trait
*/


trait Equal[T] {
  
  def apply(a:T, b:T):Boolean
  
}





















/**
 demo Equal companion
*/


object Equal {
  
  def apply[T](f:(T,T) => Boolean) = new Equal[T] {
    def apply(a:T, b:T):Boolean = f(a,b)
  }
  
  def scalaEq[T] = new Equal[T] {
    def apply(a:T, b:T):Boolean = a == b
  }
  
  implicit val int = scalaEq[Int]
  implicit val boolean = scalaEq[Boolean]
  implicit val string = scalaEq[String]
  
  implicit def list[T](implicit eq:Equal[T]) = Equal[List[T]]( (l1,l2) => l1.length == l2.length && l1.zip(l2).forall { case (a,b) => eq(a,b) } )
  
  implicit def option[T](implicit eq:Equal[T]) = Equal[Option[T]]( (l1,l2) => if (l1.isEmpty == l2.isEmpty)  l1.zip(l2).forall { case (a,b) => eq(a,b) } else false)
  
  implicit def pair[T1,T2](implicit eq1:Equal[T1], eq2:Equal[T2]) = Equal[(T1,T2)]( (a,b) => eq1(a._1, b._1) && eq2(a._2, b._2) )
}




















/**
 demo MonoidLaws
*/


object MonoidLaws {
  
  def associative[T](a:T,b:T,c:T)(implicit monoid:Monoid[T], eq:Equal[T]):Boolean =
    eq( monoid.append( monoid.append(a, b), c), monoid.append( a, monoid.append(b, c)))

  def identity[T](a:T)(implicit monoid:Monoid[T], eq:Equal[T]):Boolean =
    eq( monoid.append(a, monoid.zero), a) && eq( a, monoid.append(a, monoid.zero))
    
}
























/**
 demo ScalaCheck
*/


import org.scalacheck._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

class MyChecks extends Properties("Some checks") {
  
  property("string concat") = forAll( (a:String, b:String) =>
    (a.length + b.length) == (a.concat(b)).length
  )
  
}





















/**
 demo ScalaCheck custom types
*/


case class Data(i:Int) {
  def abs = Math.abs(i)
}

class MyChecks1 extends Properties("Some checks 1") {
  
  implicit val arbitraryData = Arbitrary(for {
  	i <- Arbitrary.arbitrary[Int]
  } yield Data(i))
  
  property("data abs property") = forAll( (d:Data) =>
    d.abs >= 0
  )
  
}





















/**
 MonoidLawsCheck.check
*/


class MonoidLawsCheck[T](implicit monoid: Monoid[T], eq: Equal[T], arb:Arbitrary[T]) extends Properties("Monoid Laws") {
  
  property("associative") = forAll( (a:T, b:T, c:T) =>
    MonoidLaws.associative(a, b, c)
  )
  
  property("identity") = forAll( (a:T) =>
    MonoidLaws.identity(a)
  )
  
}

























/**
 check the laws
*/
object Sample3 {

  import FewMonoidInstances._
  
  new MonoidLawsCheck[Option[Int]].check
  new MonoidLawsCheck[Option[List[Int]]].check
  new MonoidLawsCheck[(Int, Option[List[Int]])].check
  

}






















/**
 Function1 equal
*/
object Sample4 {

  import org.scalacheck.Gen.Parameters
  
  implicit def f1Equals[A,B](implicit arbA:Arbitrary[A]):Equal[A => B] = Equal[A => B] { (f1,f2) =>
    val p = forAll { (a:A) =>
      f1(a) == f2(a)
    }
    val r = p.apply(Parameters.default)
    if (!r.success) p.check
    r.success
  }
  
  import FewMonoidInstances._
  
  new MonoidLawsCheck[Option[Int => Int]].check

}









