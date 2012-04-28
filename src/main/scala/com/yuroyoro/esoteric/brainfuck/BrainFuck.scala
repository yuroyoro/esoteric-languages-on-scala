package com.yuroyoro.esoteric.brainfuck

import com.yuroyoro.esoteric.parser._

/**
 * Brainf*ck派生言語のジェネレータ
 */

object BrainFuck {
  import scala.util.parsing.input.{Position, NoPosition}

  sealed abstract class Op {
    def exec( runtime:Runtime )
    val token:String
    val pos:Position
    override def toString = token
  }

  case class Inc( token:String, pos:Position ) extends Op {
    def exec( runtime:Runtime ) = runtime.inc
  }
  case class Dec( token:String, pos:Position ) extends Op{
    def exec( runtime:Runtime ) = runtime.dec
  }
  case class Next( token:String, pos:Position ) extends Op{
    def exec( runtime:Runtime ) = runtime.next
  }
  case class Prev( token:String, pos:Position ) extends Op{
    def exec( runtime:Runtime ) = runtime.prev
  }
  case class Get( token:String, pos:Position ) extends Op{
    def exec( runtime:Runtime ) = runtime.get
  }
  case class Put( token:String, pos:Position ) extends Op{
    def exec( runtime:Runtime ) = runtime.put
  }
  case class NoOp() extends Op{
    val token = ""
    val pos:Position = NoPosition
    def exec( runtime:Runtime ) = {}
  }
  case class Loop( op:List[Op], val pos:Position ) extends Op{
    val token = ""
    override def toString = "[" + op.mkString + "]"
    def exec( runtime:Runtime ) = {
      var cnt = 0
      while( runtime.m != 0 ){
        if( cnt > runtime.maxLoop )
          throw BFException("ループの実行回数が最大値を超えました。最大値:{%s}".format( runtime.maxLoop ))
        op.foreach( _.exec( runtime ) )
        cnt = cnt + 1
      }
    }
  }

  case class BFException(msg:String) extends java.lang.RuntimeException(msg)

  class Runtime( val op:List[Op], val size:Int, val maxLoop:Int ){
    val offset = size / 2

    var mem = new Array[Int]( size )
    var pt = 0
    def p = pt + offset

    def inRange_? = if( pt < 0 || pt > size) throw BFException(
           "ポインタの位置がメモリの範囲を超えました。ポインタは-%s - %sの範囲にある必要があります。ポインタ位置:{%s}".format(offset, offset, p ))

    def m = {
      inRange_?
      mem(p)
    }
    def m( b:Int ) = {
      inRange_?
      mem(p) = b
    }

    def inc  = { m( m + 1 ) }
    def dec  = { m( m - 1 ) }
    def next = { pt = pt + 1 }
    def prev = { pt = pt - 1 }
    def get = m( readChar.toInt )
    def put = print( m.toChar )

    def reset = {
      pt = 0
      mem = new Array[Int]( size )
    }

    def run = op.foreach{ e =>
      try{ e.exec( this ) }
      catch{ case BFException( msg ) => throw BFException("""line:%s column %s : %s""".format( e.pos.line, e.pos.column, msg))
      }
    }
  }

  class Parser(
    incTokens:List[String],
    decTokens:List[String],
    nextTokens:List[String],
    prevTokens:List[String],
    putTokens:List[String],
    getTokens:List[String],
    startTokens:List[String],
    endTokens:List[String],
    size:Int, maxLoop:Int
  )extends EsotericParser[Op, Runtime] {

    def inc :Parser[Op] = (tks(incTokens))  ^^ { case ~(p,x) => Inc( x, p ) }
    def dec :Parser[Op] = (tks(decTokens))  ^^ { case ~(p,x) => Dec( x, p ) }
    def next:Parser[Op] = (tks(nextTokens)) ^^ { case ~(p,x) => Next( x, p ) }
    def prev:Parser[Op] = (tks(prevTokens)) ^^ { case ~(p,x) => Prev( x, p ) }
    def put :Parser[Op] = (tks(putTokens))  ^^ { case ~(p,x)  => Put( x, p ) }
    def get :Parser[Op] = (tks(getTokens))  ^^ { case ~(p,x) => Get( x, p ) }

    def start :Parser[Op] = (tks(startTokens)) ^^ ( x => NoOp() )
    def end   :Parser[Op] = (tks(endTokens))   ^^ ( x => NoOp() )

    def token       :Parser[Op] = inc  ||| dec ||| next |||
                                  prev ||| get ||| put

    def loop        :Parser[Op] = wrap( start ~> rep(instruction) <~ end ) ^^ {
     case ~(p,op) => new Loop( op ,p )
    }

    def commentOp:Parser[Op] = not( token | start | end ) <~ any ^^ ( x => NoOp() )
    def instruction :Parser[Op] = loop | token | commentOp
    def brainfuck   :Parser[Runtime] = rep(instruction) ^^ {op => new Runtime(op, size, maxLoop) }
    def prog = brainfuck

    def run(s:String) = parse( s ) foreach{ _.run }

    def genHelloWorld(sep:String) = {
      "+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+.".toList.map {
       case '+' => incTokens.head
       case '-' => decTokens.head
       case '>' => nextTokens.head
       case '<' => prevTokens.head
       case '.' => putTokens.head
       case ',' => getTokens.head
       case '[' => startTokens.head
       case ']' => endTokens.head
      }.mkString( sep )

    }
  }
}
