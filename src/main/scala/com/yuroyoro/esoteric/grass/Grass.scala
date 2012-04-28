package com.yuroyoro.esoteric.grass

import com.yuroyoro.esoteric.parser._

object Grass {
  import scala.util.parsing.input.{Position, NoPosition}

  sealed abstract class Insn extends ( CED => CED ){
    val pos:Position
  }
  case class App( m:Int, n:Int, pos:Position ) extends Insn{
    override def apply( ced:CED ) =
      ced.e( m - 1 )( ced.e( n - 1 ), ced )

    override def toString = "App(%s,%s)".format(m, n)
  }
  case class Abs( m:Int, body:List[App] ,pos:Position ) extends Insn{
    override def apply( ced:CED ) = m match {
      case 1 => CED( ced.c, Fn( body, ced.e ) :: ced.e, ced.d )
      case _ => CED( ced.c, Fn( Abs( m - 1, body, pos ) :: Nil, ced.e ) :: ced.e, ced.d )
    }
    override def toString = "Abs(%s)".format(m)
  }

  case class CED( c:List[Insn], e:List[Value], d:List[CE] )
  case class CE( c:List[Insn], e:List[Value] )

  class Runtime(val insn:List[Insn]) {

    val e0 = Out :: Succ :: CharFn('w') :: In :: Nil
    val d0 = CE(Nil, Nil) :: CE( App(1, 1, NoPosition) :: Nil, Nil) :: Nil

    def run:Unit = {
      var c = eval( CED( insn, e0, d0 ) )
      while( c != None ){
        val Some(m) = c
        c = eval( m )
      }
    }

    def eval( ced:CED ) = ced.c match {
      case Nil => ced.d match {
        case Nil => None
        case x::xs  => Some( CED( x.c, ced.e.head:: x.e , xs ))
      }
      case code :: remains => Some( code( CED( remains, ced.e, ced.d )) )
    }
  }

  abstract class Value extends ( (Value, CED) => CED )
  case class Fn(code : List[Insn], env : List[Value]) extends Value {
    override def apply( v:Value, ced:CED ) =
      CED( code , v :: env, CE( ced.c, ced.e ) :: ced.d )
    override def toString = "Fn"
  }
  case class CharFn(char : Char) extends Value {
    val ChurchTrue  = Fn(
      Abs( 1, App( 3, 2, NoPosition ) :: Nil, NoPosition ) :: Nil,
           Fn( Nil, Nil ) :: Nil )
    val ChurchFalse = Fn( Abs( 1, Nil,  NoPosition) :: Nil,  Nil)

    override def apply( v:Value, ced:CED ) = v match {
      case CharFn( c ) =>
        CED( ced.c, ced.e ::: ( if( char == c ) ChurchTrue else ChurchFalse ) :: Nil, ced.d )
      case _ => throw new Exception("eval error value is not CharFn")
    }
    override def toString = "CharFn(%s, %s)".format( char , char.toInt)
  }
  object Succ extends Value {
    override def apply( v:Value, ced:CED ) = v match {
      case CharFn( c ) =>
        val char = ( (c + 1) % 256 ).toChar
        CED( ced.c, CharFn( char ) :: ced.e, ced.d )
      case _ => throw new Exception("eval error value is not CharFn")
    }
    override def toString = "Succ"
  }
  object Out extends Value {
    override def apply( v:Value, ced:CED ) = v match {
      case CharFn( c ) =>
        print(c)
        CED( ced.c, v :: ced.e, ced.d )
      case _ => throw new Exception("eval error value is not CharFn")
    }
    override def toString = "Out"
  }
  object In extends Value {
    override def apply( v:Value, ced:CED ) ={
      val c = readChar
      CED( ced.c, CharFn( c ) :: ced.e, ced.d )
    }
    override def toString = "In"
  }

  trait BaseParser extends EsotericParser[String, Runtime] {
    def wToken :Parser[String]
    def fToken :Parser[String]
    def vToken :Parser[String]

    def w :Parser[String] = rep( comment ) ~> wToken <~ rep( comment )
    def f :Parser[String] = rep( comment ) ~> fToken <~ rep( comment )
    def v :Parser[String] = rep( comment ) ~> vToken <~ rep( comment )

    def token   :Parser[String] = wToken ||| fToken ||| vToken

    def app :Parser[App] = wrap( rep1(f) ~ rep1(w) ) ^^
      { case ~( p, x ~ y ) => App( x.size, y.size, p ) }

    def abs :Parser[Abs] = wrap( rep1(w) ~ rep(app) ~ rep(v) ) ^^
      { case ~( p, ws ~ body ~ vs ) => Abs( ws.size, body, p ) }

    def prog :Parser[Runtime] = rep( abs ) ~ rep( app ) ~ rep( v ) ^^
      { case a ~ p ~ v => new Runtime(a ::: p) }

    def run( s:String ) = parse(s) foreach{ _.run }

    def test( s:String ) = parse(s) foreach{ r => dump(r.insn, 0 ) }

    def dump( x:List[Insn] , n:Int ):Unit = {
      val sp = (for( i <- 0 to n ) yield{ "  " } ).mkString
      x.foreach{ o => o match {
        case Abs( i,b,_ ) => {
          println( sp + "Abs( " + i + ")")
          dump( b , n + 1 )
        }
        case App( i,j,_) => println( sp + "App( " + i + ", " + j + " )")
      }}
    }
  }

  class Parser(
    wTokens:List[String],
    fTokens:List[String],
    vTokens:List[String]
  ) extends BaseParser {
    def wToken = make(wTokens)
    def fToken = make(fTokens)
    def vToken = make(vTokens)
  }

  class Home2LangParser(tokenString:String) extends BaseParser {
    def wToken = tokenString.r
    val sep = """ """.r
    def fToken = rep1(sep) ~> rep1(wToken) <~ rep1(sep) ^^ { x => "W" * x.length }
    def vToken = """[\t\n]""".r
    override def token   :Parser[String] = wToken ||| fToken ||| vToken
    override def app :Parser[App] = wrap(f ~ rep1( w ) ) ^^
      { case ~( p, x ~ y ) => App( x.size, y.size, p ) }

    override def abs :Parser[Abs] = wrap(rep1(w) ~ rep(app) ~ rep(v) ) ^^
      { case ~( p, ws ~ body ~ vs ) => Abs( ws.size, body, p ) }

  }
}
