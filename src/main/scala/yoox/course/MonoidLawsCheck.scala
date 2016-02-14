package yoox.course

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.scalacheck.Gen.Parameters


class MyChecks extends Properties("Some checks") {
  
  property("string concat") = forAll( (a:String, b:String) => 
    (a.length + b.length) == (a.concat(b)).length 
  ) 
  
}

class MonoidLawsCheck[T](implicit monoid: yoox.course.Monoid[T], eq: yoox.course.Equal[T], arb:Arbitrary[T]) extends Properties("Monoid Laws") {
  
  property("associative") = forAll( (a:T, b:T, c:T) => 
    Monoid.Laws.associative(a, b, c) 
  )
  
  property("identity") = forAll( (a:T) => 
    Monoid.Laws.identity(a)
  ) 
  
}

object MonoidLawApp extends App {
  
  new MonoidLawsCheck[Int].check
  new MonoidLawsCheck[Option[String]].check
  new MonoidLawsCheck[Option[List[Int]]].check

  val f1 = (a:Int) => a * 3
  val f2 = (a:Int) => a * a
  val f3 = (a:Int) => a - 4
  
  val f4 = Monoid.appendAll(Some(f1), None, Some(f2), Some(f3))
  val f4a = f1.andThen(f2).andThen(f3)
  
    
  forAll { (i:Int) =>
    f4.get(i) == f4a(i)
  }.check

  val f1Eq = Equal.function1[Int, Int]
  
  assert( f1Eq( (a:Int) => a * 2, (a:Int) => a * 3) )
  
  new MonoidLawsCheck[Option[Int => Int]].check
  
  
}

object MonoidLawApp1 extends App {
  
  implicit def listFold[T](implicit m:Monoid[T]) = Monoid.listFold[T]
  
  println( Monoid.append(
      List(Some("str"), None, Some("blah")), 
      List(Some("34343"), Some("asfa"), None, Some("hh"))
  ) )
  
  
  
}