package nonogram

import scala.collection.mutable.Stack
import scala.io.Source

object Main {
  def main(args: Array[String]) {
    val testBoard = new Board(5,5)
    testBoard.makeMark(0, 2, Mark.filled )
    testBoard.printBoard
    
    //	3 by 3 Board
	//  _ X _ 1
	//  X _ X 1 1
	//  _ X _ 1
    //  1 1 1 
    //	  1
    
    var RowClues: Array[Stack[Int]] = Array(Stack(1), Stack(1,1), Stack(1))
    var ColClues: Array[Stack[Int]] = Array(Stack(1), Stack(1,1), Stack(1))
    var numCols: Int = 3
    var numRows: Int = 3
	    
    //	5 by 5 Board
	//  X X X _ _ 3
	//  _ _ X _ _ 1
	//  _ _ X X X 3
    //  _ _ _ X X 2 
    //	_ X X X X 4
    //	1 1 3 3 3
    //	  1 1
    
    RowClues = Array(Stack(3), Stack(1), Stack(3), Stack(2), Stack(4))
    ColClues = Array(Stack(1), Stack(1,1), Stack(3,1), Stack(3), Stack(3))
    numCols = 5
    numRows = 5
    
    //	10 by 11 Board
	//  _ _ _ _ _ _ _ X _ X X 1 2
	//  _ _ _ _ _ _ _ _ _ X X 2
	//  _ _ _ _ _ _ _ _ X _ _ 1
    //  _ _ _ _ _ _ _ _ _ _ X 1
    //	_ _ _ _ _ X X _ _ _ _ 2
	//  X X _ _ X X X X _ _ _ 2 4
	//  X X _ X X X X X X _ _ 2 6
	//  _ X X X X X X X X _ _ 8
	//  _ _ _ _ X _ _ X _ _ _ 1 1
	//  _ _ _ X X _ X X _ _ _ 2 2
    //	2 3 1 2 5 4 4 1 1 2 2
    //	      1     1 5 2   1
    //
    RowClues = Array(Stack(1,2), Stack(2), Stack(1), Stack(1), Stack(2),
        Stack(2,4), Stack(2,6), Stack(8), Stack(1,1), Stack(2,2))
    ColClues = Array(Stack(2), Stack(3), Stack(1), Stack(2,1), Stack(5),
        Stack(4), Stack(4,1), Stack(1,5), Stack(1,2), Stack(2), Stack(2,1))
    numCols = 11
    numRows = 5

    
    
  }
}