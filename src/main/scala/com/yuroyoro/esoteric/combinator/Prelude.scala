package com.yuroyoro.esoteric.combinator

trait Prelude extends Combinator {
  // bool
  val T = K     // k
  val F = K(I)  // `ki

  val IF  = I                      // i
  val NOT = S(S(I)(K(K(I))))(K(K)) // ``s``si`k`ki`kk
  val AND = S(S)(K(K(K(I))))       // ``ss`k`k`ki
  val OR  = S(I)(K(K))             // ``si`kk

  // list
  // ``s``s`ks``s`kk``s`ks``s`k`sik`kk;
  val CONS = S(S(K(S))(S(K(K))(S(K(S))(S(K(S(I)))(K)))))(K(K))
  // ``si`kk
  val CAR  = S(I)(K(K))
  // ``si`k`ki
  val CDR  = S(I)(K(K(I)))
  // `kk
  val NIL  = K(K)
  // ``si`k`k`k`ki
  val NILP = S(I)(K(K(K(K(I)))))
  // ``s`k`s`k``si`kk``si`k``si`k`ki
  val NTH  = S(K(S(K(S(I)(K(K))))))(S(I)(K(S(I)(K(K(I))))))

  // num
  // `ki
  val ZERO = K(I)
  // `s``s`ksk
  val SUCC = S(S(K(S))(K))
  // ``si`k`s``s`ksk
  val ADD  = S(I)(K(S(S(K(S))(K))))
  // ``s`ksk
  val MUL  = S(K(S))(K)
  // ``s`k`sik
  val POW  = S(K(S(I)))(K)
  // ``s``s`ks``s`k`s`ks``s``s`ks``s`k`s`ks``s`k`s`kk``s``s`ksk`k``s`k`s`k`si``s`k`s`kk``s`k`sik`k`kk`k`k`ki
  val PRED = S(S(K(S))(S(K(S(K(S))))(S(S(K(S))(S(K(S(K(S))))(S(K(S(K(K))))(S(S(K(S))(K))(K(S(K(S(K(S(I)))))(S(K(S(K(K))))(S(K(S(I)))(K)))))))))(K(K(K))))))(K(K(K(I))))
  // ``s`k`s``si`k``s``s`ks``s`k`s`ks``s``s`ks``s`k`s`ks``s`k`s`kk``s``s`ksk`k``s`k`s`k`si``s`k`s`kk``s`k`sik`k`kk`k`k`kik
  val SUB  = S(K(S(S(I)(K(S(S(K(S))(S(K(S(K(S))))(S(S(K(S))(S(K(S(K(S))))(S(K(S(K(K))))(S(S(K(S))(K))(K(S(K(S(K(S(I)))))(S(K(S(K(K))))(S(K(S(I)))(K)))))))))(K(K(K))))))(K(K(K(I)))))))))(K)

  // Y combinator and others
  // ``s``s``s`ksk`k``sii``s``s`ksk`k``sii
  val Y = S(S(S(K(S))(K))(K(S(I)(I))))(S(S(K(S))(K))(K(S(I)(I))))
  // val Y = (f:F) => ((g:F) => (m:F) => _a(_a(f(g(g)))(m))((g:F) => (m:F) => _a(f(g(g)))(m)))

  val IFR = (p:F) => (x:F) => (y:F) => _a(_a(_a(IF(p))(x))(y))(I)
  val LSOF = (a:F) => Y(CONS(a))
  val CNEQ = (m:F) => (n:F) => _a(NTH(n))(_a(m(CONS(F)))(_a(CONS(T))(LSOF(F))))
  val CNNE = (m:F) => (n:F) => _a(NTH(n))(_a(m(CONS(T)))(_a(CONS(F))(LSOF(T))))
}
