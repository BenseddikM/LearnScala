object lecture2_5{
  class Rational(x: Int, y: Int) {
    def numer = x
    def denom = y

    def add(that: Rational) =
      new Rational(numer * that.denom + that.numer * denom, denom * that.denom)
    def neg() =
      new Rational(- numer, denom)
    def sub(that:Rational) =
      new Rational(numer * that.denom - that.numer * denom, denom * that.denom)


    override def toString: String = numer + "/" + denom
  }

  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)
  x.numer
  x.denom

  def makeString(r: Rational)=
    r.numer + "/" + r.denom

  x.neg()
  x.add(y)
  x.sub(y)
  x.sub(y).sub(z)
}