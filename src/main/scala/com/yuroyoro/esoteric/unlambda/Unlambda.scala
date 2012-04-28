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
}

object Unlambda extends Combinator {
  var __debug = false

  import scala.util.parsing.combinator._
  import scala.util.parsing.input.{Position, NoPosition}

  // Unlambda(`r`````````````.H.e.l.l.o., . .w.o.r.l.d.!.a)
  class Runtime {
    var _c:Char = _ // current character
    def _o(f:F, cs:List[Char]) =  Option((f, cs))

    // void
    val V:F = (x:A) => V _
    // print
    val P = (c:Char) => (x:A) => {print(c); x}
    // carriage return
    val M = (x:A) => {print('\n');x}
    // read
    val R = (x:A) => { _c = readChar; _c }
    // reprint character read
    val N = (x:A) => if(_c == 0) V else {print(_c);_c}
    // compare character read
    val X = (c:Char) => (x:A) => if(_c == c) x else V
    // exit
    val E = (x:A) => Unit
    // delay
    val D:F = (x:A) => D _

    def eval(cs:List[Char]):Option[(A,List[Char])] = cs match {
      case '`' :: xs => for((l,ys) <- eval(xs);(r,zs) <- eval(ys)) yield { (_a(l)(r),zs) } // apply
      case 'i' :: xs => _o(I,xs)                // identity
      case 'k' :: xs => _o(K,xs)                // constant generator
      case 's' :: xs => _o(S,xs)                // substitution
      case '@' :: xs => _o(R,xs)                // read
      case '|' :: xs => _o(N,xs)                // reprint character read
      case 'v' :: xs => _o(V,xs)                // void
      case 'e' :: xs => _o(E,xs)                // exit
      case 'd' :: xs => _o(D,xs)                // delay
      case 'r' :: xs => _o(M,xs)                // carriage return
      // case 'c' :: xs =>                         // call with current continuation
      case '.' :: c :: xs => _o(P(c),xs)        // print
      case '?' :: c :: xs => _o(X(c),xs)        // compare character read
      case ' ' :: xs => eval(xs)
      case _        => None
    }
    def run(source:String)(x:Any) = (eval(source.toList).map{case (f,_) => _a(f)}.getOrElse{ sys.error("parse error") })(x)
  }

  class Parser(rt:Runtime) extends RegexParsers {
    import scala.util.parsing.input.CharSequenceReader._
    override def skipWhitespace = false

    abstract class Tr {
      def run:F
      val token:String
      val pos:Position
      override def toString = token
    }

    case class Lf(e:F, token:String, pos:Position) extends Tr {
      if(__debug) println("%s:%s" format(token, pos))
      def run:F = e
    }
    case class Br(l:Tr,r:Tr, token:String, pos:Position) extends Tr {
      if(__debug) println("%s:%s" format(token, pos))
      def run:F = _a(l.run(r.run))
      override def toString = "%s(%s)" format(l,r)
    }

    def p(s:String):Parser[String] = s
    def wrap[A](p: Parser[A]) = Parser{r => Success(r.pos,  r)} ~ p
    def t(s:String) = wrap(p(s))

    val char:Parser[Char] = elem("", _ != EofCh) ^^ { _.toChar }
    val any :Parser[String] = elem("", _ != EofCh) ^^ { _.toString }

    def i:Parser[Tr] = t("i") ^^ { case p~x => Lf(I,x,p) }
    def k:Parser[Tr] = t("k") ^^ { case p~x => Lf(K,x,p) }
    def s:Parser[Tr] = t("s") ^^ { case p~x => Lf(S,x,p) }
    def ap:Parser[Tr] = t("`") ~ token  ~ token ^^ { case p~x~l~r => Br(l,r,x,p) }

    def rr:Parser[Tr] = t("@") ^^ { case p~x => Lf(rt.R,x,p) }
    def rn:Parser[Tr] = t("|") ^^ { case p~x => Lf(rt.N,x,p) }
    def rv:Parser[Tr] = t("v") ^^ { case p~x => Lf(rt.V,x,p) }
    def re:Parser[Tr] = t("e") ^^ { case p~x => Lf(rt.E,x,p) }
    def rd:Parser[Tr] = t("d") ^^ { case p~x => Lf(rt.D,x,p) }
    def rm:Parser[Tr] = t("r") ^^ { case p~x => Lf(rt.M,x,p) }
    def rp:Parser[Tr] = t(".") ~ char ^^ { case p~x~c => Lf(rt.P(c),x,p) }
    def rx:Parser[Tr] = t("?") ~ char ^^ { case p~x~c => Lf(rt.X(c),x,p) }

    def token:Parser[Tr] = i ||| k ||| s ||| ap ||| rr ||| rn ||| rv ||| re ||| rd ||| rm ||| rp ||| rx
    def comment:Parser[String] = not( token ) <~ any ^^ ( (Unit) => "" )
    def prog:Parser[Tr] = ap

    def parse(s:String):Option[Tr] = parseAll(prog, s) match {
      case Success( tr, _ )  => Option(tr)
      case Failure( msg, _ ) => { println( msg ); None }
      case Error( msg, _ )   => { println( msg ); None }
    }
    def apply(source:String) = parse(source).map(_.run)
  }

  def runtime = new Runtime
  def parser = new Parser(runtime)

  def code(source:String) = parser.parse(source)
  def apply(source:String)(x:Any) = _a(code(source).map{_.run}.getOrElse{sys.error("parse error")})(x)
  def dump(source:String) = code(source).get.toString.toUpperCase
}
// Unlambda("`r`````````````.H.e.l.l.o.,. .w.o.r.l.d.!.a")

object Main extends App {
  val hw = "`r`````````````.H.e.l.l.o.,. .w.o.r.l.d.!.a"
  val parser = Unlambda.parser

  println("print \"``skk(100)\"")
  println(parser("``skk").map{f => f(100)})
  println("print \"hello world\"")
  println(parser(hw))
}

