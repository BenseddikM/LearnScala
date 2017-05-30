object lecture2_5{
  class Rational(x: Int, y: Int) {
    require(y != 0, "denominator must be different that 0 !")

    def this(x: Int) = this(x,1)

    private def gcd(a :Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    def numer = x
    def denom = y

    def + (that: Rational) =
      new Rational(numer * that.denom + that.numer * denom, denom * that.denom)
    def unary_- : Rational =
      new Rational(- numer, denom)
    def - (that:Rational) = this + -that
    def < (that: Rational) = numer * that.denom < that.numer * denom
    def max(that: Rational) = if (this < that) that else this

    override def toString: String = {
      val g = gcd(x,y)
      numer / g + "/" + denom / g
    }
  }

  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)
  val a = new Rational(1)
  a.denom
  // val strange = new Rational(1,0)
  // strange.add(strange)
  x.numer
  x.denom

  def makeString(r: Rational)=
    r.numer + "/" + r.denom

  -x
  x + y
  x - y
  x - y - z
  x < y
  x max y

  val r = 1
  // assert(r <= 0)


  new Rational(1,2) < new Rational(2,3)

}