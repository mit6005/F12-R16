/**
  Exercises for the 6.005 Recitation on advanced Scala concepts.
  Jean Yang (jeanyang [at] mit)
  November 2012

  You will want to load your definitions from SimpleEval.scala before loading
  this file.  You may want to copy over your SimpleEval.scala that you
  completed from the last recitation!

  We have the following exercises.
  1. Thought question: what is the expected syntax for input expressions?
  2. Try using the "apply" function to parse well-formed and poorly-formed
     arithmetic expressions.  What currently happens if you try parsing
     Boolean expressions? 
  3. Add parsing support for "and" and "or" operators.
  4. Add parsing support for Boolean expressions.

  You may find it useful to read the documentation for the RegexParser libary
  here:
  http://www.scala-lang.org/api/current/scala/util/parsing/combinator/RegexParsers.html
*/
import scala.util.parsing.combinator.RegexParsers

object ExprParser extends RegexParsers {
  /* Parsing operators.  These parsers succeed if the input matches the string
     and then return what is after the ^^^^. */
  val plus  = "+" ^^^ Plus
  val minus = "-" ^^^ Minus
  val times = "*" ^^^ Times
  val div   = "/" ^^^ Div
  val operator: Parser[Operator] = plus | minus | times | div

  /* Integer expressions.  The ^^ allows us to pattern match against the
     input parsed in order to transform it into another format. */
  def intConstant: Parser[IntExpr] = """\d+(\.\d*)?""".r ^^
    { v => IntConstant(v.toInt) }
  def intBinop  : Parser[IntExpr] =
    intExpr~operator~intExpr ^^ { case i1~op~i2 => IntBinop(op, i1, i2) }
  def intExpr: Parser[IntExpr] = intConstant | "(" ~> intBinop <~ ")"

  /* Boolean expressions. */
  def boolExpr: Parser[BoolExpr] = throw Unimplemented

  def expr: Parser[Expr] = intExpr | boolExpr

  /* The parseAll function takes an input and parses it against our expr
     parser to see if parsing succeeded.  To understand how this function
     works, you may want to read the documention for parseAll and also for
     the ParseResult type. */
  def apply(input: String): Expr = parseAll(expr, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}
