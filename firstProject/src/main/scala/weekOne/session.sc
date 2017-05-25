import scala.annotation.tailrec

object session {
  1 + 2

  def abs(x: Double) = if (x < 0) -x else x

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)

    def isGoodEnough(guess: Double, x: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double, x: Double) =
      (guess + x / guess) / 2


    sqrtIter(1.0, x)
  }

  sqrt(2)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)


  val x = 0

  def f(y: Int) = y + 1

  val result = {
    val x = f(3);
    x * x
  } + x

  val y = x + 1;
  y * y


  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  gcd(14, 21)


  def factorial(n: Int): Int =
    if (n == 0) 1 else n * factorial(n - 1 )


}