package yoox.course.slides.monoids;

















/**

 In scala there are two ways to provide parameters to a function/method:

*/






























/**

 1) explicit parameters

*/
object Sample1 {

  def f(a:Int, b:Int) = a + b
  
  println( f(2,3) )

}


























/**

 2) implicit parameters:

*/
object Sample2 {

  def f(a:Int)(implicit b:Int) = a + b
  
  implicit val defaultBValue = 10
  
  println( f(3) )

}

























/**

Scala compiler discover implicit values in the following way (simplified):

First look in current scope
  Implicits defined in current scope
  Explicit imports
  wildcard imports

Now look at associated types in
  Companion objects of a type
  Outer objects for nested types

*/

























/**

 Implicits defined in current scope

*/
object Sample3 {

  def f(a:Int)(implicit b:Int) = a + b
  
  implicit val defaultBValue = 10
  
  println( f(3) )

}























/**

 Explicit imports

*/
object Sample4 {

  object MyImplicits {
    implicit val defaultBValue = 10
  }
  
  def f(a:Int)(implicit b:Int) = a + b
  
  import MyImplicits.defaultBValue
  
  println( f(3) )

}





















/**

 Wildcard  imports

*/
object Sample5 {

  object MyImplicits {
    implicit val defaultBValue = 10
  }
  
  def f(a:Int)(implicit b:Int) = a + b
  
  import MyImplicits._
  
  println( f(3) )

}





















/**

 Companion objects of a type

*/
object Sample6 {

  class MyClass(val i:Int)
  
  object MyClass {
    implicit val defaultBValue = new MyClass(10)
  }
  
  def f(a:Int)(implicit b:MyClass) = a + b.i
  
  println( f(3) )

}





















/**

 Outer objects for nested types

*/
object Sample7 {

  object MyOuter {
  
    implicit val defaultBValue = new MyClass(10)
    
    class MyClass(val i:Int)
  }
  
  def f(a:Int)(implicit b:MyOuter.MyClass) = a + b.i
  
  println( f(3) )

}

























/**

Those example, as most guides/tutorial on the internet fail to show the real power that lies in implicit values:

implicits composition


*/






























/**

demo:

	monoids

	monoid for:Int,String,List[T], Option[T],


*/





























/**

demo:

	Monoid.append function

	monoid for:(A,B), T => T


*/

























/**

demo:

	Equal
	
	MonoidLaws
	 
		associativity
		identity
		
	ScalaCheck

	




*/










