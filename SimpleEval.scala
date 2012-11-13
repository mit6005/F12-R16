/**
  Exercises for the 6.005 Recitation on Scala data types and pattern matching.
  Jean Yang (jeanyang [at] mit)
  November 2012

  Things we will do in recitation:
  - Review map, filter, and reduce from last time.
  - Learn about data types and pattern matching in order to complete the
    implementation of a simple evaluator.

  Next time, we'll write a parser for this thing.
*/

/* PART ONE: Map/filter/reduce discussion exercises.
   1. How do we implement map and filter using reduce?
   2. What happens if we use foldRight as reduce instead of foldLeft?
   3. Other questions from last lecture.
 */

/* PART TWO: Simple arithmetic evaluator.
   0. Get familiar looking at the expressions in the EvalTest object.  Call
      "eval" and "show" on them and see what happens.  For instance, try:
        EvalTest.t
        SimpleEval.eval(EvalTest.t)
        SimpleEval.show(EvalTest.t)
   1. Define evaluation for the arithmetic operations "Minus," "Times," and
      "Div."  (Hint: it should look a lot like the implementation for "Plus.")
   2. Add case objects extending the Operator class for the Boolean operations
      "And" and "Or."  When you try to compile/load this, you will find that
      you will also have to change one other part of the code.
   3. Implement the evaluation of Boolean binary operations.
   BONUS: Add string expressions.
*/
sealed abstract class Expr
sealed abstract class IntExpr extends Expr
sealed abstract class BoolExpr extends Expr

/* Operators. */
sealed abstract class Operator
case object Plus extends Operator
case object Minus extends Operator
case object Times extends Operator
case object Div extends Operator
/* Exercise 2: Extend this with "And" and "Or." */

case class IntConstant(v: Int) extends IntExpr
case class IntBinop(op: Operator, e1: IntExpr, e2: IntExpr) extends IntExpr

case class BoolConstant(v: Boolean) extends BoolExpr
case class BoolBinop(op: Operator, e1: BoolExpr, e2: BoolExpr) extends BoolExpr

case object Unimplemented extends Exception
case class Unexpected(msg: String) extends Exception

object SimpleEval {
  /* Evaluation functions. */
  def eval(e: Expr): Expr = {
    e match {
      // Here's a cool thing: you can also pattern match on *types*!
      case (i: IntExpr) => evalIntExpr(i)
      case (b: BoolExpr) => evalBoolExpr(b)
    }
  }
  
  /* Evaluating integer expressions. */
  def evalIntExpr(e: IntExpr): IntExpr =
    e match {
      case IntConstant(_v) => e
      // Exercise 1: Define evaluation for the other arithmetic operators.
      case IntBinop(op, e1, e2) =>
        op match {
          case Plus =>
            (eval(e1), eval(e2)) match {
              case (IntConstant(i1), IntConstant(i2)) => IntConstant(i1 + i2)
              case _ => throw Unexpected("adding two non-integers")
            }
          case Minus => throw Unimplemented
          case Times => throw Unimplemented
          case Div => throw Unimplemented
        }
    }

  /* Evaluating boolean expressions. */
  def evalBoolExpr(e: BoolExpr): BoolExpr =
    e match {
      case BoolConstant(_v) => e
      /* Exercise 4: write evaluation for Boolean binary expressions. */
      case BoolBinop(op, e1, e2) => throw Unimplemented
    }

  /* Printing functions.  I have put there here for your interpreter-using
     pleasure.  Note that here, print is overloaded for different subtypes of
     Expr. */
  def show(op: Operator): String =
    op match {
      case Plus => "+"
      case Minus => "-"
      case Times => "*"
      case Div => "/"
    }
  def show(e: IntExpr): String =
    e match {
      case IntConstant(v) => v.toString()
      case IntBinop(op, e1, e2) =>
        "(" ++ show(e1) ++ " " ++ show(op) ++ " " ++ show(e2) ++ ")"
    }
  def show(e: BoolExpr): String =
    e match {
      case BoolConstant(v) => v.toString()
      case BoolBinop(op, e1, e2) =>
        "(" ++ show(e1) ++ " " ++ show(op) ++ " " ++ show(e2) ++ ")"
    }
  }
}

/* I have defined some expressions here for your testing pleasure.  These should
   help you write less in the interpreter. */
object EvalTest {
  /* Some constants */
  val zero = IntConstant(0)
  val one = IntConstant(1)
  val two = IntConstant(2)
  val three = IntConstant(3)

  val t = BoolConstant(true)
  val f = BoolConstant(false)

  def add(e1: IntExpr, e2: IntExpr) = IntBinop(Plus, e1, e2)
  def sub(e1: IntExpr, e2: IntExpr) = IntBinop(Minus, e1, e2)
  def times(e1: IntExpr, e2: IntExpr) = IntBinop(Times, e1, e2)
  def div(e1: IntExpr, e2: IntExpr) = IntBinop(Div, e1, e2)
}
