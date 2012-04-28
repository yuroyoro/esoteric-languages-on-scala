package com.yuroyoro.esoteric.grass

object HomuApp extends SampleApp {
  import Grass._
  lazy val wTokens = List("")
  lazy val fTokens = List("")
  lazy val vTokens = List("")

  lazy override val parser = new Home2LangParser("ほむ")
  lazy val programName = "プログラミング言語 ほむほむ"


  lazy val printW = "ほむ ほむほむ ほむほむほむほむ"
  lazy val printX = "ほむ ほむほむほむ ほむほむほむほむ ほむほむほむ ほむ"
  lazy val helloworld= scala.io.Source.fromFile(new java.io.File( "./home2lang.grass" )).mkString
  // lazy val helloworld = """ほむ
// ほむ ほむ ほむほむほむほむ ほむ ほむほむほむほむほむほむ ほむほむほむほむほむ ほむ ほむほむほむほむほむ ほむほむほむほむ
// ほむほむほむほむ ほむほむほむ ほむほむ ほむ ほむほむ ほむほむほむほむほむほむ ほむほむほむほむ ほむ ほむほむ
// ほむ ほむほむ ほむ ほむ ほむほむ
// ほむほむ ほむほむ ほむ
// ほむ ほむほむほむ ほむほむ ほむほむほむほむほむ ほむほむほむ ほむ ほむほむ ほむほむほむほむほむほむ ほむ ほむほむほむほむほむほむほむ ほむ ほむほむほむほむ ほむ ほむほむほむほむほむ ほむ ほむほむほむほむほむほむ ほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむほむほむほむほむほむほむほむ ほむ ほむ ほむ ほむ ほむほむほむ ほむほむ ほむほむほむほむ ほむほむほむほむほむほむほむ ほむほむほむほむほむ ほむほむほむほむほむほむほむ ほむほむほむほむほむほむ ほむほむほむほむほむほむほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむ ほむほむほむほむほむほむ ほむほむ ほむほむほむほむほむほむ ほむほむほむ ほむほむほむほむほむほむ ほむほむほむほむほむほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむ ほむほむほむほむほむほむほむほむほむほむ ほむほむ ほむほむほむほむ ほむほむほむ ほむほむほむほむ ほむほむほむほむ ほむ ほむほむほむほむほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむほむほむほむほむほむほむほむほむほむほむ ほむ ほむ ほむほむほむ ほむほむ ほむ ほむほむほむ ほむ ほむほむほむほむ ほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむ ほむほむほむほむほむほむほむほむほむほむほむ ほむほむ ほむほむほむほむほむほむほむほむほむほむほむ ほむほむほむ ほむほむほむほむほむほむほむ ほむほむほむほむ ほむ ほむほむほむほむほむ ほむほむほむほむほむほむほむほむ ほむほむほむほむほむほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむほむほむほむほむほむほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむほむほむほむほむほむほむほむほむ ほむほむほむほむほむほむほむほむ ほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむほむほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむほむほむほむ ほむほむほむほむほむほむほむほむ ほむほむほむほむほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむ ほむほむほむほむほむほむ ほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむほむ"""
}
