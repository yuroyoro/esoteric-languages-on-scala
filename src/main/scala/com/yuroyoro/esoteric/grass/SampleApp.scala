package com.yuroyoro.esoteric.grass

trait SampleApp extends App {
  import Grass._

  val programName:String

  val wTokens:List[String]
  val fTokens:List[String]
  val vTokens:List[String]

  val helloworld:String
  val printW:String
  val printX:String

  val p = println(_:String)
  def line = p("-" * 80)
  def cr = p("")

  lazy val parser:BaseParser = new Parser(wTokens, fTokens, vTokens)
  def test(s:String) = parser.test(s)
  def run(s:String)  = parser.run(s)

  line
  p(programName)
  line

  cr
  p( "print w" )
  p("Source:")
  p(printW)
  cr
  p("Ast:")
  test(printW)
  cr
  p("Result:")
  run(printW)
  cr
  line

  cr
  p( "print x" )
  p("Source:")
  p(printX)
  cr
  p("Ast:")
  test(printX)
  cr
  p("Result:")
  run(printX)
  cr
  line

  cr
  p("Hello World")
  p("Source:")
  p(helloworld)
  cr
  p("Ast:")
  test(helloworld)
  cr
  p("Result:")
  run(helloworld)
  cr
  line
}
