package funsets

object Main extends App {

  import FunSets._

  println("Singleton Test :")
  println("contains(singletonSet(1), 1) = " + contains(singletonSet(1), 1))


  val unionSet = union(singletonSet(1), singletonSet(2))
  println("Union Tests :")
  println("contains(unionSet,1) = " + contains(unionSet, 1))
  println("contains(unionSet,2) = " + contains(unionSet, 2))
  println("contains(unionSet,3) = " + contains(unionSet, 3))

  println("Diff Tests :")
  val diffSet = diff(singletonSet(1), singletonSet(2))
  println("contains(diffSet,1) = " + contains(diffSet, 1))
  println("contains(diffSet,2) = " + contains(diffSet, 2))
  println("contains(diffSet,3) = " + contains(diffSet, 3))


}
