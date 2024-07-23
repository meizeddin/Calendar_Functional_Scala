package topics

import lib.parser.Parser.*
import lib.parser.ParserLib.*
import lib.logic.L3.*
import lib.logic.L3Parser.*

object Unit08 {
  /*
   * Exercise 81
  */
  val p81a: Parser[Char] = char('{') >> getc >>= (c => char('}') >> result(c))
  lazy val p81b: Parser[Char] = for _ <- char('{') ; c <- getc; _ <- char('}') yield c
  lazy val p81c: Parser[String] = for _ <- char('<'); c <- many(sat( c => c !=('<') && c !=('>'))); _ <- char('>') yield c.mkString
  lazy val p81d: Parser[String] = for _ <- string("<TD>"); s <- many(sat( s => !s.equals("<TD>") && !s.equals("</TD>"))); _ <- string("</TD>") yield s.mkString

  @main def exercise81(): Unit =
    println(p81a.parse("{a}"))
    println(p81b.parse("{b}"))
    println(p81c.parse("<HTML>"))
    println(p81d.parse("<TD>An item</TD>"))
//    println(p81d.parse("<TD></TD>"))
//    println(p81d.parse("<TD>Mismatched</TR>"))


  /*
   * Exercise 82
  */

  // recognise a single bit, 0 or 1, returning the corresponding Int value
  def bit: Parser[Int] = ???

  // recognise a sequence of bits, e.g. 1111, returning the result as a sequence of Int (0 or 1) values
  def bits: Parser[Seq[Int]] = ???

  // Convert the result from the bits parser to a single Int value
  def binary: Parser[Int] = ???

  @main def exercise82(): Unit =
//    println(binary.parse(""))
//    println(binary.parse("0"))
//    println(binary.parse("1"))
//    println(binary.parse("11111111"))
//    println(binary.parse("1111111111111111"))
//    println(binary.parse("111111111111111111111111"))
//    println(binary.parse("11111111111111111111111111111111"))
//    println(binary.parse("10000000000000000000000000000000"))
//    println(binary.parse("01111111111111111111111111111111"))
    ()

  /*
   * Exercise 83
  */
  @main def exercise83(): Unit =
    // Repeat exercise 71 but use strings to enter the formulae and use
    // L3Parser.formula to translate them. The first one is provided to
    // get you started.

    val f: Formula = formula.parse("A & B").get
    val env: Map[TruthVar, TruthVal] = Map(A -> U, B -> F)
    println(f)
    println(f.showTruthTable)
    println(s"$f.evaluate($env) = ${f.evaluate(env)}")

    // etc.


  /*
   * Exercise 84
  */

  case class Money(dollars: Int, cents: Int) {
    override def toString: String = f"$$$dollars.$cents%02d"

    def +(that: Money): Money =
      val c = this.cents + that.cents
      Money(this.dollars + that.dollars + c / 100, c % 100)

    def *(n: Int): Money =
      val result = (dollars * 100 + cents) * n
      Money(result / 100, result % 100)
  }

  def money: Parser[Money] =
    for _ <- char('$').sp
        d <- nat.sp
        _ <- char('.').sp
        c <- nat.sp
    yield Money(d, c)

  def sums: Parser[Money] = money.sp.someWith(char('+').sp).map(_.reduce(_ + _))

  def term: Parser[(Money, Int)] =
    (for m <- money.sp; _ <- char('*').sp; n <- nat.sp yield (m, n)) <+> money.sp.map((_, 1))

  def sumz: Parser[Money] =
    term.sp.someWith(char('+').sp).map(_.map(_ * _).reduce(_ + _))

  def sumzz: Parser[Money] = ???

  @main def exercise84(): Unit =
    println(money.parse("$6.50").get + money.parse("$3.50").get)
    println(sums.parse("$1.10 + $2.22 + $3.33 + $4.44").get)
    println(sumz.parse("$1.10 * 2 + $2.22 + $3.33 * 3 + $4.44").get)
    println(sumz.parse("$1.10 * 10").get)
}
