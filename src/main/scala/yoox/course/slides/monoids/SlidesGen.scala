package yoox.course.slides.monoids

import scala.io.Source
import scala.util.Try
import scala.util.Success

case class SlidesGenConf(pageSize: Int = 37)

sealed trait SlideSeparator
case object CommentsSlideSeparator extends SlideSeparator
case object CodeSlideSeparator extends SlideSeparator
case object RawCodeSlideSeparator extends SlideSeparator

sealed trait SlideKind
case object CommentsSlideKind extends SlideKind
case object CodeSlideKind extends SlideKind
case object RawCodeSlideKind extends SlideKind

final case class Slide(kind:SlideKind, lines: Seq[Line]) {
  def isEmpty = lines.isEmpty
  def +(line: Line) = Slide(kind, lines :+ line)
}

sealed trait Line {
  def value:String
  def isComment = this match {
    case CommentLine(_) => true
    case _ => false
  }
}
final case class CodeLine(value: String) extends Line
final case class CommentLine(value: String) extends Line

class SlidesGen(conf: SlidesGenConf) {

  def getSeparator(line: String): Option[SlideSeparator] = {
    val str = line.trim
    if (str.length < 4) None
    else {
      val hd = str.headOption
      hd match {
        case Some('*') if str.tail.forall(ch => ch == '-') => Some(CommentsSlideSeparator)
        case Some('-') if str.tail.forall(ch => ch == '-') => Some(CodeSlideSeparator)
        case Some('r') if str.tail.forall(ch => ch == '-') => Some(RawCodeSlideSeparator)
        case _ => None
      }
    }
  }

  def parseSlideContent(lines: Seq[String]): (Seq[String], Option[(SlideSeparator, Seq[String])]) = {
    val z: (Seq[String], Option[(SlideSeparator, Seq[String])]) = (Nil, None)
    lines.foldLeft(z) { (acc, ln) =>
      val lastNonWhitespace = ln.lastIndexWhere((ch:Char) => !Character.isWhitespace(ch))
      val line = (if (lastNonWhitespace > -1) ln.substring(0, lastNonWhitespace+1) else ln).replace("\n", "")
      acc._2 match {
        case Some((sep, lines)) => acc._1 -> Some(sep -> (lines :+ line))
        case _ =>
          getSeparator(line) match {
            case None => (acc._1 :+ line) -> None
            case Some(sep) => acc._1 -> (Some(sep -> Nil))
          }
      }
    }
  }
  
  def slideKind(sep: SlideSeparator) = sep match {
    case CodeSlideSeparator => CodeSlideKind
    case RawCodeSlideSeparator => RawCodeSlideKind
    case CommentsSlideSeparator => CommentsSlideKind
  }

  def parseSlides(sep: SlideSeparator, lines: Seq[String]): (Seq[Slide], Option[(SlideSeparator, Seq[String])]) = {
    val (lns, rest) = parseSlideContent(lines)
    val sknd = slideKind(sep)
    val slide = sep match {
      case x @ (CodeSlideSeparator | RawCodeSlideSeparator) =>
        Slide(sknd, lns.map {
          case ln if ln.trim.startsWith("*") => CommentLine(ln.substring(ln.indexOf("*")+1))
          case ln => CodeLine(ln)
        })
      case CommentsSlideSeparator =>
        Slide(sknd, lns.map(CommentLine(_)))
    }

    rest match {
      case None => Seq(slide) -> None
      case Some((sep, lines)) =>
        val rest = parseSlides(sep, lines)
        (slide +: rest._1) -> rest._2
    }
  }

  def parseSlides(source: Source): Seq[Slide] = {
    val lines = source.getLines().toSeq
    val r = lines match {
      case Seq() => Nil -> None
      case hd +: rest =>
        getSeparator(hd) match {
          case None => parseSlides(CodeSlideSeparator, lines)
          case Some(sep) => parseSlides(sep, rest)
        }
    }
    assert(r._2.isEmpty)
    r._1
  }
  
  def toScalaSource(source:Source):String = {
    val slides = parseSlides(source)
    toScalaSource( slides ) 
  }
  
  def toScalaSource(slides:Seq[Slide]):String = {
    val counter = newCounter() 
    slides.flatMap(toScalaSource(counter, _:Slide)).mkString("\n")
  }

  def newCounter():() => Int = {
    val intRef = new java.util.concurrent.atomic.AtomicInteger(1)
    () => intRef.getAndIncrement
  }

  lazy val startOfComment = Seq("/**")
  lazy val endOfComment = Seq("*/")
  
  def toScalaSource(slide:Slide):Seq[String] = {
    val counter = newCounter()
    toScalaSource( counter, slide )
  }
  def toScalaSource(counter:() => Int, slide:Slide):Seq[String] = {

    lazy val endOfCode = if (slide.kind == RawCodeSlideKind) Seq("") else Seq("", "}")
    lazy val codeIndent = if (slide.kind == RawCodeSlideKind) "" else "  "
    
    val z:(Boolean, Boolean, Seq[String]) = (false, true, Nil)
    
    val (isComment, _, cont) = slide.lines.foldLeft(z) { (acc, line) =>
      val isComment = acc._1
      val isFirst = acc._2
      val lns = (isComment -> line.isComment) match {
        case (true,true) => acc._3 ++ Seq(line.value)
        case (false, false) => acc._3 ++ Seq(codeIndent+line.value)
        case (false, true) => {
          acc._3 ++ (if (isFirst) startOfComment else endOfCode ++ startOfComment) :+ line.value 
        }
        case (true, false) => {
          val startOfCode = if (isFirst || slide.kind == RawCodeSlideKind) "" else s"object Sample${counter()} {"
          acc._3 ++ endOfComment ++ Seq(startOfCode, line.value)
        }
      }
      (line.isComment, false, lns)
    }
    
    val content = cont ++ (if (isComment) endOfComment else endOfCode)
    
    val padd = (conf.pageSize - content.length) / 2
    val r = (1.to(padd).map(p => "") ++ content).padTo(conf.pageSize, "")
    r
  }
  
}

object SlideGenApp extends App {
  
  val gen = new SlidesGen(SlidesGenConf(39))
  
  val pr = new java.io.PrintWriter(new java.io.File("src/main/scala/yoox/course/slides/monoids_laws/slides.scala"))
  pr.println(gen.toScalaSource(Source.fromFile(new java.io.File("monoid-laws-slides.txt"))))
  pr.close()
  
  
  
}
