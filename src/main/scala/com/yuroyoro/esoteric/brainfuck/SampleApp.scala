package com.yuroyoro.esoteric.brainfuck

trait SampleApp extends App {
  import BrainFuck._
  val size    = 30000
  val maxLoop = 65535

  val programName:String

  val incTokens   : List[String]
  val decTokens   : List[String]
  val nextTokens  : List[String]
  val prevTokens  : List[String]
  val putTokens   : List[String]
  val getTokens   : List[String]
  val startTokens : List[String]
  val endTokens   : List[String]

  val helloworld:String

  val p = println(_:String)
  def line = p("-" * 80)
  def cr = p("")

  val parser = new Parser(incTokens, decTokens, nextTokens,
    prevTokens, putTokens, getTokens, startTokens,
    endTokens, size,  maxLoop)
  def run(source:String) = parser.run(source)


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

