package yoox.course.samples.di

case class MailAddress(value:String)
case class Mail(address:MailAddress, text:String)

case class UserBorrowingBooks(name:String, age:Int, books:List[BorrowedBook])
case class BorrowedBook(title:String, author:String, date:java.util.Date)
