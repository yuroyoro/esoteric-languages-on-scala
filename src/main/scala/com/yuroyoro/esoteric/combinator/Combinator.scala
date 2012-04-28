package com.yuroyoro.esoteric.combinator

trait Combinator {
  type A = Any
  type F = A => A

  def _a(a:A) = a.asInstanceOf[F]
  // identity
  val I = (x:A) => x
  // constant generator
  val K = (x:A) => (y:A) => x
  // substitution
  val S = (x:A) => (y:A) => (z:A) => _a(_a(x)(z))(_a(y)(z)) // x(z)(y(z))
  // iota
  val U:F = (x:A) => _a(_a(x)(S))(K)
  // jot
  val B:F = (x:A) => (y:A) => (z:A) => _a(x)(_a(y)(z))
}

