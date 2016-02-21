package yoox.course.samples

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.Left
import scala.Right
import yoox.course.Applicative
import yoox.course.FreeAp
import yoox.course.HigherKind
import yoox.course.NaturalTransformation
import java.math.BigDecimal

sealed trait Property[T] {
  def name: String
}

object Property {

  case class Integer[A](name: String, f: Int => A) extends Property[A]
  case class Real[A](name: String, f: BigDecimal => A) extends Property[A]
  case class Bool[A](name: String, f: Boolean => A) extends Property[A]
  case class Text[A](name: String, f: String => A) extends Property[A]
  case class Struct[A](name: String, f: FreeAp[Property, A]) extends Property[A]
  case class Choice[A](name: String, options: Seq[FreeAp[Property, A]]) extends Property[A]

  type FreeProperty[A] = FreeAp[Property, A]

  private def lift[A](value: Property[A]): FreeProperty[A] = FreeAp.lift[Property, A](value)

  def int(field: String): FreeProperty[Int] = lift(Integer(field, identity))
  def real(field: String): FreeProperty[BigDecimal] = lift(Real(field, identity))
  def bool(field: String): FreeProperty[Boolean] = lift(Bool(field, identity))
  def string(field: String): FreeProperty[String] = lift(Text(field, identity))
  def struct[A](field: String, value: FreeProperty[A]) = lift(Struct(field, value))
  def choice[A](field: String, options: Seq[FreeProperty[A]]) = lift(Choice(field, options))

  lazy val PropertyFactory = Applicative[FreeProperty]
  
}

object PropertySample extends App {
  
  import Property._
  
  case class Person(name:String, age:Int)
  case class Pet(name:String, fur:String, owner:Person)

  val person = PropertyFactory.map2(string("name"), int("age"))(Person)

  val pet = PropertyFactory.map3(string("name"), string("fur"), struct("owner", person))(Pet)
  
  println(PropertyMapReadInterpreter(person, Map("name" -> "pippo", "age" -> 27)))
  
  println(PropertyMapReadInterpreter(pet, Map("name" -> "Fido", "fur" -> "Red", "owner" -> Map("name" -> "pippo", "age" -> 27))))
  
  println(PropertyDocInterpreter(pet).mkString("\n"))
  
}

object PropSample1 extends App {
  
  import Property._
  
  case class Person(name:String, pet:Pet)
  
  sealed trait Pet
  case class Dog(name:String, fur:String) extends Pet
  case class Fish(color:String) extends Pet
  
  val fish = PropertyFactory.map(string("color"))(Fish).map( p => p:Pet)
  val dog = PropertyFactory.map2(string("name"), string("fur"))(Dog).map( p => p:Pet)

  val person = PropertyFactory.map2(string("name"), choice("pet", Seq(fish, dog)))(Person)

  println(PropertyMapReadInterpreter(person, Map("name" -> "pippo", "pet" -> Map("color" -> "Red"))))
  println(PropertyMapReadInterpreter(person, Map("name" -> "minni", "pet" -> Map("name" -> "Fido", "fur" -> "Yellow"))))
  println(PropertyDocInterpreter(person).mkString("\n"))
}

object PropertyMapReadInterpreter { intepreter =>
  
  import Property._
  
  case class Errors(errors:Seq[Throwable]) extends Exception(errors.map(_.getMessage).mkString("\n"))

  private def getValue[T](mp:Map[String, Any], name:String):Try[T] =
    Try(mp(name).asInstanceOf[T])
  
  def apply[T](mapping:FreeProperty[T], map:Map[String, Any]):Try[T] = {
    mapping.foldMap(new NaturalTransformation[Property, Try] {
      def apply[T](i: Property[T]): Try[T] = {
        i match {
          case Integer(name, f) => getValue[Int](map, name).map(f)
          case Real(name, f) => getValue[BigDecimal](map, name).map(f) 
          case Text(name, f) => getValue[String](map, name).map(f) 
          case Bool(name, f) => getValue[Boolean](map, name).map(f) 
          case Struct(name, f) =>
            getValue[Map[String,Any]](map, name).flatMap { v =>
              intepreter(f, v)              
            }
            
          case Choice(name, options) => 
            getValue[Map[String,Any]](map, name).flatMap { mapEntry =>
              options.foldLeft(Left(Nil):Either[List[Throwable], T]) { (acc,prop) =>
                acc match {
                  case Left(errs) => intepreter(prop, mapEntry) match {
                    case Success(v) => Right(v)
                    case Failure(err) => Left(errs :+ err) 
                  }
                  case _ => acc
                }
              } match {
                case (Right(v)) => Success(v)
                case (Left(errs)) => Failure(Errors(errs))
              }
            } 
        }
      }
    })
  }
  
}


object PropertyDocInterpreter { intepreter =>
  
  import Property._
  import HigherKind._
  
  implicit val monApp:Applicative[Const[List[String]]#Type] = Applicative.monoid[List[String]]
  
	def apply[T](mapping:FreeProperty[T]):List[String] = {
    apply(mapping, 0)
  }
  def apply[T](mapping:FreeProperty[T], indent:Int):List[String] = {
    mapping.foldMap[Const[List[String]]#Type](new NaturalTransformation[Property, HigherKind.Const[List[String]]#Type] {
      def apply[T](i: Property[T]): List[String] = {
        val indentStr = "  "*indent
        def pad(s:String) = s.padTo(8, " ").mkString
        (i match {
          case Integer(name, f) => List(s"${pad(name)}:Int")
          case Real(name, f) => List(s"${pad(name)}:BigDecimal")
          case Text(name, f) => List(s"${pad(name)}:String")
          case Bool(name, f) => List(s"${pad(name)}:Boolean")
          case Struct(name, f) => 
            List(s"${pad(name)}: {") ++ intepreter(f, indent+1) ++ List("}")
          case Choice(name, options) => 
            List(s"${pad(name)}:choice(") ++ options.zipWithIndex.flatMap {  case (f,idx) => s"  case $idx:" +: intepreter(f, indent+2) } ++ List(")")
        }).map( indentStr + _ )
      }
    })
  }
  
}







