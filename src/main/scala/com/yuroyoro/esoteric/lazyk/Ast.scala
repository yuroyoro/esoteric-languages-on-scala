package com.yuroyoro.esoteric.lazyk

import com.yuroyoro.esoteric.combinator._
import scala.util.parsing.input.{Position, NoPosition}

trait Ast extends Combinator {
  var __debug = false

  abstract class Tr {
    def run:F
    val token:String
    val pos:Position
    override def toString = token
    def _a(a:A) = a.asInstanceOf[F]
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
}
