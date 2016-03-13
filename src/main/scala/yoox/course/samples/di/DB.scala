package yoox.course
package samples.di

import java.sql.Connection
import yoox.course.ReaderMonad
import scala.util.Try
import scala.util.Success

trait DB {
  def createConnetion:Connection
}

object DB {
  type Fun[T] = Kleisli[Try, DB, T]
  
  implicit def funMonad = Monad.kleisliMonad[Try,DB]
  
  def none[T] = Kleisli.const[Try,DB,Option[T]](None)
  
  case class UserId(value:String)
  case class BookId(value:String)
  case class BorrowedBookId(value:String)
  
  case class User(id:UserId, name:String, email:MailAddress)
  case class Book(id:BookId, title:String, author:String)
  case class BorrowedBook(id:BorrowedBookId, user:UserId, book:BookId, startDate:java.util.Date)

}

trait DAO[ID,V] {
  
  import DB.Fun
  
  def all:Fun[List[V]]
  def byId(id:ID):Fun[Option[V]]
  
  def save(user:V):Fun[Unit]
  
}

trait UserDAO extends DAO[DB.UserId, DB.User] {

  import DB._
  
  def byName(name:String):Fun[User]
  
  def setAge(id:UserId, mailAddress:MailAddress) = for {
    u <- byId(id)
    saved <- u.map( u => save(User(u.id, u.name, mailAddress)) ).getOrElse(none)
  } yield u

}

trait BookDAO extends DAO[DB.BookId, DB.Book] {
  
  import DB._
  
  def byAuthor(name:String):Fun[List[Book]]
  
}

trait BorrowedBooksDAO extends DAO[DB.BorrowedBookId, DB.BorrowedBook]  {
  import DB._
  
  def bookIdsByUser(u:UserId):Fun[List[BookId]]
  
}

case class Persistence(users:UserDAO, books:BookDAO, borrowedBooks:BorrowedBooksDAO ) {
  
  def booksByUser(u:DB.UserId):DB.Fun[List[DB.Book]] = for {
    bookIds <- borrowedBooks.bookIdsByUser(u)
    bks0:List[Kleisli[Try, DB, List[DB.Book]]] = bookIds.map( id => books.byId(id).map(_.toList))
    bks <- Traverse[List].sequence[DB.Fun, List[DB.Book]]( bks0 )
  } yield bks.flatten
  
}

trait MailServer {
  def send(mail:Mail)
}

object MailServer {
  
  type Fun[T] = Kleisli[Try, MailServer, T]

}

trait MailService {
  
  def sendMail(mail:Mail):MailServer.Fun[Unit] = Kleisli { mailServer =>
    Try(mailServer.send(mail))
  }
}

trait AppConfig {
  
  def db:DB
  def mailServer:MailServer
  
}

object AppConfig {
  
  type Fun[T] = Kleisli[Try,AppConfig,T]
  
}

case class Application(mailService:MailService, persistence:Persistence) {
  
  def sendBooksListToUser(name:String):AppConfig.Fun[Unit] = for {
    user <- persistence.users.byName(name).local[AppConfig](_.db)
    books <- persistence.booksByUser(user.id).local[AppConfig](_.db)
    res <- mailService.sendMail(Mail(user.email, books.toString)).local[AppConfig](_.mailServer)
  } yield res
  
  
}

