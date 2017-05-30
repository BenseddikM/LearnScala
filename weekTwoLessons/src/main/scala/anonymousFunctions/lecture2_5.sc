object lecture2_5{
  class Rational(x: Int, y: Int) {
    require(y != 0, "denominator must be different that 0 !")

    def this(x: Int) = this(x,1)

    private def gcd(a :Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    def numer = x
    def denom = y

    def add(that: Rational) =
      new Rational(numer * that.denom + that.numer * denom, denom * that.denom)
    def neg() =
      new Rational(- numer, denom)
    def sub(that:Rational) =
      new Rational(numer * that.denom - that.numer * denom, denom * that.denom)
    def less(that: Rational) = numer * that.denom < that.numer * denom
    def max(that: Rational) = if (this.less(that)) that else this

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

  x.neg()
  x.add(y)
  x.sub(y)
  x.sub(y).sub(z)
  x.less(y)
  x.max(y)

  val r = 1
  assert(r <= 0)


  val bigRational = new Rational(1221212,323233)
  bigRational.numer
}