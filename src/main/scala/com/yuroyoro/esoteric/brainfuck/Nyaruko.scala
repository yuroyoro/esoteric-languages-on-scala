package com.yuroyoro.esoteric.brainfuck

object NyarukoApp extends SampleApp {

  // masarakki/nyaruko_lang : https://github.com/masarakki/nyaruko_lang
  lazy val programName = "名状しがたいプログラミング言語のようなもの Nyaruko"

  lazy val incTokens   = List( "(」・ω・)」うー!(/・ω・)/にゃー" )
  lazy val decTokens   = List( "(」・ω・)」うー!!!(/・ω・)/にゃー!!" )
  lazy val nextTokens  = List( "(」・ω・)」うー(/・ω・)/にゃー" )
  lazy val prevTokens  = List( "(」・ω・)」うー!!(/・ω・)/にゃー!" )
  lazy val putTokens   = List( "Let's＼(・ω・)／にゃー"  )
  lazy val getTokens   = List( "cosmic!"  )
  lazy val startTokens = List( "CHAOS☆CHAOS!"  )
  lazy val endTokens   = List( "I WANNA CHAOS!"  )


  lazy val helloworld = """(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃーCHAOS☆CHAOS!(」・ω・)」うー(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!!(/・ω・)/にゃー!(」・ω・)」うー!!(/・ω・)/にゃー!(」・ω・)」うー!!(/・ω・)/にゃー!(」・ω・)」うー!!!(/・ω・)/にゃー!!I WANNA CHAOS!(」・ω・)」うー(/・ω・)/にゃーLet's＼(・ω・)／にゃー(」・ω・)」うー(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃーLet's＼(・ω・)／にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃーLet's＼(・ω・)／にゃーLet's＼(・ω・)／にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃーLet's＼(・ω・)／にゃー(」・ω・)」うー(/・ω・)/にゃー(」・ω・)」うー!!!(/・ω・)/にゃー!!Let's＼(・ω・)／にゃー(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!Let's＼(・ω・)／にゃー(」・ω・)」うー!!(/・ω・)/にゃー!(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃーLet's＼(・ω・)／にゃー(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!Let's＼(・ω・)／にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃーLet's＼(・ω・)／にゃー(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!Let's＼(・ω・)／にゃー(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!(」・ω・)」うー!!!(/・ω・)/にゃー!!Let's＼(・ω・)／にゃー(」・ω・)」うー(/・ω・)/にゃー(」・ω・)」うー!(/・ω・)/にゃーLet's＼(・ω・)／にゃー
"""

}

