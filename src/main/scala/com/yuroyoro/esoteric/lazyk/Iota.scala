package com.yuroyoro.esoteric.lazyk

object Iota extends Abs {

  class Runtime extends AbsRuntime {
    val evalPf:LC --> Result = {
      case '*' :: xs => for((l,ys) <- eval(xs);(r,zs) <- eval(ys)) yield { (_a(l)(r),zs) } // apply
      case 'i' :: xs => _o(U,xs)
      case _        => None
    }
  }

  class Parser(apChars:List[String] = List("*"),
    uChars:List[String] = List("i")) extends AbsParser {

    def ap:Parser[Tr] = wrap(make(apChars)) ~ token  ~ token ^^ { case p~x~l~r => Br(l,r,x,p) }
    def u: Parser[Tr] = wrap(make(uChars)) ^^ { case p~x => Lf(U,x,p) }

    def token:Parser[Tr] = ap ||| u
    def prog:Parser[Tr] = ap
  }

  def runtime:AbsRuntime = new Runtime
}

