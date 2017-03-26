package calculator

import scala.math._
import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  test("compute values") {
    val a = Ref("a")
    val b = Literal(1.0)
    val plus = Plus(a, b)

    val d = Minus(Literal(5.0), Literal(3.0))

    val map = Map[String, Signal[Expr]]("a" -> Signal(plus), "d" -> Signal(d))
    val result: Map[String, Signal[Double]] = Calculator.computeValues(map)
    val r1: Signal[Double] = result("a")
    assert(Double.NaN.equals(result("a")()))
    assert(result("d")() === 2)
  }

  test("eval") {
    val a = Literal(8)
    val b = Literal(3)
    val div = Divide(a, b)
    val r = Calculator.eval(div, Map())
    assert(r === 8.0 / 3)
  }

  test("compute solutions") {
    // (-b ± √Δ) / 2a
    val a = -5.3
    val b = 4.0
    val c = 1.0
    val d = 37.2
    val p = (-b + sqrt(d)) / (2 * a)
    val n = (-b - sqrt(d)) / (2 * a)
    val s = Set(p, n)

    val sa = Signal(a)
    val sb = Signal(b)
    val sc = Signal(c)
    val sd = Signal(d)

    val result = Polynomial.computeSolutions(sa, sb, sc, sd)
    assert(result() === s)

  }

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

}
