package com.yuroyoro.esoteric.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position, NoPosition}

trait EsotericParser[A,B]  extends RegexParsers {
  import scala.util.parsing.input.CharSequenceReader._
  override def skipWhitespace = false
  var __debug = false

  def p(s:String):Parser[String] = s
  def wrap[T](p: Parser[T]) = Parser{r => Success(r.pos,  r)} ~ p
  def t(s:String) = wrap(p(s))
  def tks(tk:List[String]) = wrap(make(tk))

  def make(tk:List[String]) = ( p( tk.head ) /: tk.tail ){ _ ||| p( _ ) }

  val char:Parser[Char]   = elem("", _ != EofCh) ^^ { _.toChar }
  val any :Parser[String] = elem("", _ != EofCh) ^^ { _.toString }

  def token:Parser[A]
  def prog:Parser[B]

  def comment:Parser[String] = not( token ) <~ any ^^ ( (Unit) => "" )

  def parse(s:String):Option[B] = parseAll(prog, s) match {
    case Success(res, _ )  => Option(res)
    case Failure(msg, _ ) => { println( msg ); None }
    case Error(msg, _ )   => { println( msg ); None }
  }
}
