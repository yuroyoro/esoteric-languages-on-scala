package com.yuroyoro.esoteric.lazyk

import com.yuroyoro.esoteric.combinator._

trait IotaSampleApp extends App with Combinator{
  import Iota._

  val programName:String

  val us:String
  val ns:String

  val parser = new Parser(List(us), List(ns))
  def run(s:String) = p(cl2str(_a(_a(parser.parse(s).map{_.run}.get)(""))))

  val helloworld:String

  val p = println(_:String)
  def line = p("-" * 80)
  def cr = p("")

  line
  p(programName)
  line

  cr
  p("Hello World")
  p("Source:")
  p(helloworld)
  cr
  p("Result:")
  run(helloworld)
  cr
  line
}
