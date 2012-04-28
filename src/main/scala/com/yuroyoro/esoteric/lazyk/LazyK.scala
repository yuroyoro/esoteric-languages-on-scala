package com.yuroyoro.esoteric.lazyk

import com.yuroyoro.esoteric.combinator._
import com.yuroyoro.esoteric.parser._

/*
object Iota extends Primitives {
  var __debug = false

  class Runtime {
    def _o(f:F, cs:List[Char]) =  Option((f, cs))

    def eval(cs:List[Char]):Option[(A,List[Char])] = cs match {
      case '*' :: xs => for((l,ys) <- eval(xs);(r,zs) <- eval(ys)) yield { (_a(l)(r),zs) } // apply
      case 'i' :: xs => _o(U,xs)
      case _        => None
    }
    def run(source:String)(x:Any) = (eval(source.toList).map{case (f,_) => _a(f)}.getOrElse{ sys.error("parse error") })(x)
  }

  class Parser(rt:Runtime, apChars:List[String] = List("*"), uChars:List[String] = List("i")) extends RegexParsers {
    import scala.util.parsing.input.CharSequenceReader._
    override def skipWhitespace = false

    abstract class Tr {
      def run:F
      val token:String
      val pos:Position
      override def toString = token
    }

    case class Lf(e:F, token:String, pos:Position) extends Tr {
      if(__debug) println("%s:%s" format(token, pos))
      def run:F = e
    }
    case class Br(l:Tr,r:Tr, token:String, pos:Position) extends Tr {
      if(__debug) println("%s:%s" format(token, pos))
      def run:F = _a(l.run(r.run))
      override def toString = "%s(%s)" format(l,r)
    }
    def p(s:String):Parser[String] = s
    def wrap[A](p: Parser[A]) = Parser{r => Success(r.pos,  r)} ~ p
    def t(s:String) = wrap(p(s))
    def make( tk:List[String] ) = ( p( tk.head ) /: tk.tail ){ _ ||| p( _ ) }
    val char:Parser[Char] = elem("", _ != EofCh) ^^ { _.toChar }
    val any :Parser[String] = elem("", _ != EofCh) ^^ { _.toString }

    def ap:Parser[Tr] = wrap(make(apChars)) ~ token  ~ token ^^ { case p~x~l~r => Br(l,r,x,p) }
    def u: Parser[Tr] = wrap(make(uChars)) ^^ { case p~x => Lf(U,x,p) }

    def token:Parser[Tr] = ap ||| u
    def comment:Parser[String] = not( token ) <~ any ^^ ( (Unit) => "" )
    def prog:Parser[Tr] = ap

    def parse(s:String):Option[Tr] = parseAll(prog, s) match {
      case Success( tr, _ )  => Option(tr)
      case Failure( msg, _ ) => { println( msg ); None }
      case Error( msg, _ )   => { println( msg ); None }
    }
    def apply(source:String) = parse(source).map(_.run)
  }

  def runtime = new Runtime
  def parser(apChars:List[String] = List("*"), uChars:List[String] = List("i")) = new Parser(runtime, apChars, uChars)
  def code(source:String) = parser().parse(source)
  def apply(source:String)(x:Any) = _a(code(source).map{_.run}.getOrElse{sys.error("parse error")})(x)
}

object Jot extends Primitives {
  import scala.util.parsing.combinator._
  import scala.util.parsing.input.{Position, NoPosition}

  class Runtime {
    def eval(v:A, cs:List[Char]):(A,List[Char]) = cs match {
      case '0' :: xs => eval(U(v), xs)
      case '1' :: xs => eval(B(v), xs)
      case Nil => (v, Nil)
    }

    def run(source:String)(x:Any) = eval(I, source.toList) match { case (f,_) => _a(f)(x) }
  }

  def runtime = new Runtime
}

object LazyK extends Combinator {
  var __debug = false

  import scala.util.parsing.combinator._
  import scala.util.parsing.input.{Position, NoPosition}

  class Runtime {
    def _o(f:F, cs:List[Char]) =  Option((f, cs))

    def eval(cs:List[Char]):Option[(A,List[Char])] = cs match {
      case '`' :: xs => for((l,ys) <- eval(xs);(r,zs) <- eval(ys)) yield { (_a(l)(r),zs) } // apply
      case 'i' :: xs => _o(I,xs)                // identity
      case 'k' :: xs => _o(K,xs)                // constant generator
      case 's' :: xs => _o(S,xs)                // substitution
      case _        => None
    }
    def run(source:String)(x:Any) = (eval(source.toList).map{case (f,_) => _a(f)}.getOrElse{ sys.error("parse error") })(x)
  }
}

object Main extends App with Primitives {
  println("-" * 80)
  println("Iota hello world")
  println("")
  println("Source:")
  println(hw_iota)
  println("")

  val parser = Iota.parser()
  val hw = Iota.apply(hw_iota)("")
  println("Result:")
  println(cl2str(_a(hw)))
  println("")
  println("-" * 80)

  println("")
  println("(」・ω・)」うー！(／・ω・)／にゃー！ hello world")
  println("")
  println("Source:")
  val us = "(」・ω・)」うー！"
  val ns = "(／・ω・)／にゃー！"
  val u_hw = hw_iota.replaceAll("\\*", us).replaceAll("i", ns)
  println(u_hw)
  println("")
  val u_parser = Iota.parser(List(us), List(ns))
  val u_res = _a(u_parser.parse(u_hw).map{_.run}.get)("")
  println("Result:")
  println(cl2str(_a(u_res)))
  println("")
  println("-" * 80)

  println("")
  println("(」・ω・)」うー！(／・ω・)／にゃー その2 ！ hello world")
  println("")
  println("Source:")
  val us2 = "(」・ω・)」うー！(／・ω・)／にゃー！"
  val ns2 = " "
  val u_hw2 = hw_iota.replaceAll("\\*", us2).replaceAll("i", ns2)
  println(u_hw2)
  println("")
  val u_parser2 = Iota.parser(List(us2), List(ns2))
  val u_res2 = _a(u_parser2.parse(u_hw2).map{_.run}.get)("")
  println("Result:")
  println(cl2str(_a(u_res2)))
  println("")
  println("-" * 80)
}
*/

/*
`k``s``si`k``s`k```sii``s``s`kski``s``s`ksk``s``s`ksk```s``siii``s``s`kski`k``
s``si`k``s``s`ksk```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski`k``s`
`si`k``s`k```sii``s``s`kski```sii``s``s`ksk``s``s`kski`k``s``si`k``s`k```sii``
s``s`kski```sii``s``s`ksk``s``s`kski`k``s``si`k``s``s`ksk``s`k``s``s`kski``s``
s`ksk``s`k``s``s`kski```sii``s``s`ksk``s``s`kski`k``s``si`k````s``s`ksk```s``s
iii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski`k``s``si`k``s`k``s``s`kski`
``s``siii``s``s`kski`k``s``si`k``s`k``s``s`ksk``s`k``s``s`kski``s``s`ksk``s``s
`kski``s``s`ksk```s``siii``s``s`kski`k``s``si`k``s``s`ksk``s`k``s``s`kski``s``
s`ksk``s`k``s``s`kski```sii``s``s`ksk``s``s`kski`k``s``si`k``s`k``s``s`kski``s
``s`ksk``s`k``s``s`kski``s``s`ksk```sii``s``s`ksk``s``s`kski`k``s``si`k``s`k``
`sii``s``s`kski```sii``s``s`ksk``s``s`kski`k``s``si`k```s``s`kski``s`k``s``s`k
ski``s``s`ksk```sii``s``s`kski`k``s``si`k``s``s`ksk``s`k``s``s`kski```s``siii`
`s``s`kski`k``s``si`k``s`k``s``s`kski``s``s`ksk```sii``s``s`kski`k``s``si`k```
sii```sii``s``s`kski`k```sii```sii``s``s`kski
*/
