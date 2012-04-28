package com.yuroyoro.esoteric.lazyk

import com.yuroyoro.esoteric.combinator._
import com.yuroyoro.esoteric.parser._

trait Abs extends Ast with Primitives {
  type -->[M,N] = PartialFunction[M,N]
  type LC = List[Char]
  type Result = Option[(A,LC)]

  trait AbsRuntime {
    def _o(f:F, cs:LC) =  Option((f, cs))
    val evalPf:LC --> Result

    def eval(cs:LC):Result = evalPf(cs)

    def run(source:String)(x:Any) = (eval(source.toList).map{case (f,_) => _a(f)}.getOrElse{ sys.error("parse error") })(x)
  }

  trait AbsParser extends EsotericParser[Tr,Tr] {
    def apply(source:String) = parse(source).map(_.run)
  }

  def runtime:AbsRuntime
}

