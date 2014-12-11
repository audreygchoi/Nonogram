package nonogram
import scala.io.Source
import scala.collection

class nonogram{
  val N = 9
  //could refactor Int here to be a small "clue" class with a "color" and "size"
  var hclues: List[List[Int]] = List.empty
  var vclues: List[List[Int]] = List.empty

  class board{
    val unknown = 0
    val known = 1
    val grid = Array.ofDim[Int](N,N)
  }
}