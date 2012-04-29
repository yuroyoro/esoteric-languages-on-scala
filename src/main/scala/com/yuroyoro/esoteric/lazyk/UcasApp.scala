package com.yuroyoro.esoteric.lazyk

import com.yuroyoro.esoteric.combinator._

object UcasApp extends App with Combinator{
  import Iota._

  lazy val programName = "UCAS"

  lazy val us = "*"
  lazy val ns = "i"

  val parser = new Parser(List(us), List(ns))

  def fill( s:String, fill:String, n:Int ) = {
    val m = s.length % n
    val p = if(m == 0) 0 else n - m
    s + ( fill * p)
  }

  def binaryString2Int(l:String) = l.toList.reverse.zipWithIndex.foldLeft(0){ case( n, (c, i)) => n + (scala.math.pow( 2, i) * c.asDigit ).toInt }

  def ucas2iota(s:String) = s.map{_.toInt - 5121}.map{c => fill(c.toBinaryString.reverse, "0", 9).reverse}.mkString.dropWhile(_ == '0').mkString.collect{ case '1' => '*'; case '0' => 'i' }.mkString

  def iota2ucals(s:String) = {
    def _w(res:String, b:String):String = b.splitAt(9) match {
      case (a, "") => res + ((binaryString2Int(a) + 5121).toChar)
      case (a, b ) => _w(res + ((binaryString2Int(a) + 5121).toChar), b)
    }
    val b = fill(s.collect{ case '*' => '1'; case 'i' => '0'}.reverse.mkString, "0", 9).reverse
    _w("", b)
  }


  lazy val helloworld = iota2ucals(iota_hw)
  def run(s:String) = p(cl2str(_a(_a(parser.parse(ucas2iota(s)).map{_.run}.get)(""))))

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


  lazy val iota_hw = "**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii***i*i*i*ii*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii*****i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii***i*i*i*ii*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii***i*i*i*ii*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii***i*i*i*ii*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii****i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii***i*i*i*ii*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii****i*i*i*ii*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii****i*i*i*ii*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii"

}
