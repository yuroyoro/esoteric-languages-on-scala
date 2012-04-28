package com.yuroyoro.esoteric.combinator

// 404 Blog Not Found:Math - 言語はどこまで小さくなれるか - (unlambda|iota|jot) のすすめ : http://blog.livedoor.jp/dankogai/archives/51524324.html

trait Primitives extends Prelude {
  def cb2bool(p:F):Boolean = (_a(p(true))(false)).asInstanceOf[Boolean]
  def cn2num(n:F):Int = (_a(n((i:Int) => i + 1))(0)).asInstanceOf[Int]
  def ary2cl[Elem](list:List[Elem]):F = if(list.isEmpty) NIL else _a(_a(CONS(list.head))(ary2cl(list.tail)))
  def cl2ary(l:F):List[Any] = {
    val tail = _a(CDR(l))
    val head = CAR(l)

    val res = (try{ cn2num(_a(head)) >= 256 } catch{case _ => false }) || cb2bool(_a(NILP(tail)))
    if(res) head :: Nil else head :: cl2ary(tail)
  }

  // special numbers
  val cn1   = SUCC(ZERO)
  val cn2   = _a(_a(ADD(cn1))(cn1))
  val cn4   = _a(_a(MUL(cn2))(cn2))
  val cn16  = _a(_a(POW(cn2))(cn4))
  val cn256 = _a(_a(POW(cn4))(cn4))

  // numerals up to 255
  val cn = (0 to 256).scanLeft(ZERO){(l,n) => SUCC(l)}
  def num2cn(n:Int) = cn(n)

  // strings conversion
  def str2cl(s:String) = ary2cl(s.map{c => cn(c.toInt)}.toList)
  def cl2str(l:F) = cl2ary(l).map{n => cn2num(_a(n)) }.map{_.toChar}.mkString

  val helloworld = {
    implicit def a2f(a:Any) = _a(a)
    K(S(S(I)(K(S(K(S(I)(I)(S(S(K(S))(K))(I))))(S(S(K(S))(K))(S(S(K(S))(K))(S(S(I)(I))(I)(S(S(K(S))(K))(
I))))))))(K(S(S(I)(K(S(S(K(S))(K))(S(S(K(S))(K))(I)(S(K(S(S(K(S))(K))(I)))(S(S(K(S))(K))(S(I)(I)(S(S(K(S))(K))(
I)))))))))(K(S(S(I)(K(S(K(S(I)(I)(S(S(K(S))(K))(I))))(S(I)(I)(S(S(K(S))(K))(S(S(K(S))(K))(I)))))))(K(S(S(
I)(K(S(K(S(I)(I)(S(S(K(S))(K))(I))))(S(I)(I)(S(S(K(S))(K))(S(S(K(S))(K))(I)))))))(K(S(S(I)(K(S(S(K(S))(K))(S(K(S(S(K(S))(K))(
I)))(S(S(K(S))(K))(S(K(S(S(K(S))(K))(I)))(S(I)(I)(S(S(K(S))(K))(S(S(K(S))(K))(
I))))))))))(K(S(S(I)(K(S(S(K(S))(K))(S(S(I)(I))(I)(S(S(K(S))(K))(I)))(S(S(K(S))(K)))(S(I)(I)(S(S(K(S))(K))(S(S(K(S))(K))(
I)))))))(K(S(S(I)(K(S(K(S(S(K(S))(K))(I)))(S(S(I)(I))(I)(S(S(K(S))(K))(I))))))(K(S(S(I)(K(S(K(S(S(K(S))(K))(S(K(S(S(K(S))(K))(
I)))(S(S(K(S))(K))(S(S(K(S))(K))(I))))))(S(S(K(S))(K))(S(S(I)(I))(I)(S(S(K(S))(K))(I)))))))(K(S(S(I)(K(S(S(K(S))(K))(S(K(S(S(K(S))(K))(
I)))(S(S(K(S))(K))(S(K(S(S(K(S))(K))(I)))(S(I)(I)(S(S(K(S))(K))(S(S(K(S))(K))(I))))))))))(K(S(S(I)(K(S(K(S(S(K(S))(K))(
I)))(S(S(K(S))(K))(S(K(S(S(K(S))(K))(I)))(S(S(K(S))(K))(S(I)(I)(S(S(K(S))(K))(S(S(K(S))(K))(I))))))))))(K(S(S(I)(K(S(K(S(
I)(I)(S(S(K(S))(K))(I))))(S(I)(I)(S(S(K(S))(K))(S(S(K(S))(K))(I)))))))(K(S(S(I)(K(S(S(K(S))(K))(I)(S(K(S(S(K(S))(K))(
I)))(S(S(K(S))(K))(S(I)(I)(S(S(K(S))(K))(I))))))))(K(S(S(I)(K(S(S(K(S))(K))(S(K(S(S(K(S))(K))(I)))(S(S(I)(I))(I)(S(S(K(S))(K))(
I)))))))(K(S(S(I)(K(S(K(S(S(K(S))(K))(I)))(S(S(K(S))(K))(S(I)(I)(S(S(K(S))(K))(I)))))))(K(S(S(I)(K(S(I)(I)(S(
  I)(I)(S(S(K(S))(K))(I))))))(K(S(I)(I)(S(I)(I)(S(S(K(S))(K))(I))))))))))))))))))))))))))))))))))
  }

  // helloworld("") -> returns 'hello, world!' as  church numbers
  // cl2str(_a(helloworld("")))

  val hw_ski = "`k``s``si`k``s`k```sii``s``s`kski``s``s`ksk``s``s`ksk```s``siii``s``s`kski`k``s``si`k``s``s`ksk```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski`k``s``si`k``s`k```sii``s``s`kski```sii``s``s`ksk``s``s`kski`k``s``si`k``s`k```sii``s``s`kski```sii``s``s`ksk``s``s`kski`k``s``si`k``s``s`ksk``s`k``s``s`kski``s``s`ksk``s`k``s``s`kski```sii``s``s`ksk``s``s`kski`k``s``si`k````s``s`ksk```s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski`k``s``si`k``s`k``s``s`kski```s``siii``s``s`kski`k``s``si`k``s`k``s``s`ksk``s`k``s``s`kski``s``s`ksk``s``s`kski``s``s`ksk```s``siii``s``s`kski`k``s``si`k``s``s`ksk``s`k``s``s`kski``s``s`ksk``s`k``s``s`kski```sii``s``s`ksk``s``s`kski`k``s``si`k``s`k``s``s`kski``s``s`ksk``s`k``s``s`kski``s``s`ksk```sii``s``s`ksk``s``s`kski`k``s``si`k``s`k```sii``s``s`kski```sii``s``s`ksk``s``s`kski`k``s``si`k```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski`k``s``si`k``s``s`ksk``s`k``s``s`kski```s``siii``s``s`kski`k``s``si`k``s`k``s``s`kski``s``s`ksk```sii``s``s`kski`k``s``si`k```sii```sii``s``s`kski`k```sii```sii``s``s`kski"

  // i = *ii
  // k = *i*i*ii
  // s = *i*i*i*ii
  val hw_iota = "**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii***i*i*i*ii*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii*****i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii***i*i*i*ii*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii***i*i*i*ii*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii***i*i*i*ii*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii****i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii****i*i*i*ii***i*i*i*ii*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii***i*i*i*ii**i*i*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii***i*i*i*ii***i*i*i*ii*ii**i*i*ii****i*i*i*ii*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii**i*i*ii****i*i*i*ii*ii*ii****i*i*i*ii*ii*ii***i*i*i*ii***i*i*i*ii**i*i*ii*i*i*i*ii*i*i*ii*ii"
}
