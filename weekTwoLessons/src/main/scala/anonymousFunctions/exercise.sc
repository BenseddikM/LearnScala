object exercise2 {

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }

    loop(a, 0)
  }
  def sumInts(a:Int, b:Int) = sum(x => x, a, b)
  def sumCubes(a: Int, b: Int) = sum(x => x * x * x, a, b)


  def sum2(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    sumF
  }

  def sumInts2(a:Int, b:Int) = sum2(x => x)
  def sumCubes2 = sum2(x => x * x * x)
  sumInts2(1,2)

  def multiply(m: Int)(n: Int): Int = m * n
  multiply(2)(3)

  def cube = (x:Int) => x * x * x
  sum2(cube)(1,10)

  def sum3(f:Int => Int)(a: Int, b: Int): Int =
    if( a > b) 0 else f(a) + sum3(f)(a + 1, b)


}