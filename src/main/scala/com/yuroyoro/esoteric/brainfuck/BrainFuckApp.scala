package com.yuroyoro.esoteric.brainfuck

object BrainFuckApp extends SampleApp {
  import BrainFuck.BFException

  lazy val programName = "brainf*ck"

  lazy val incTokens   = List("+")
  lazy val decTokens   = List("-")
  lazy val nextTokens  = List(">")
  lazy val prevTokens  = List("<")
  lazy val putTokens   = List(".")
  lazy val getTokens   = List(",")
  lazy val startTokens = List("[")
  lazy val endTokens   = List("]")

  lazy val helloworld = """>++++++ほげほげ+++[<+++++ほげほげ+++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>++++++ほげほげ+++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]+++++ほげほげ+++++."""

  // Error
  p("Error Case")
  try{ run(">+[>]") }catch{ case BFException(msg) => println( msg ) }
  try{ run("<+[<]") }catch{ case BFException(msg) => println( msg ) }
  try{ run(">+[]")  }catch{ case BFException(msg) => println( msg ) }
}
