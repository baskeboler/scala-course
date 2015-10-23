package funsets

object Main extends App {
  import FunSets._

  override def main (args: Array[String]){
    println(contains(singletonSet(1), 1))
  }
}
