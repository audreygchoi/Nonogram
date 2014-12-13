package nonogram

import scala.io.Source

object Main {
  def main(args: Array[String]) {
    val testBoard = new Board(3,3)
    testBoard.makeMark(0, 0, Mark.blank )
    testBoard.makeMark(0, 1, Mark.filled )
    testBoard.makeMark(0, 2, Mark.blank )
//    testBoard.makeMark(1, 0, Mark.filled )
//    testBoard.makeMark(1, 1, Mark.blank )
//    testBoard.makeMark(1, 2, Mark.filled )
//    testBoard.makeMark(2, 0, Mark.blank )
//    testBoard.makeMark(2, 1, Mark.filled )
//    testBoard.makeMark(2, 2, Mark.blank )
    //testBoard.printBoard
    
    //	3 by 3 Board
	//  _ X _ 1
	//  X _ X 1 1
	//  _ X _ 1
    //  1 1 1 
    //	  1
    
    var rowClues: Array[List[Int]] = Array(List(1), List(1,1), List(1))
    var colClues: Array[List[Int]] = Array(List(1), List(1,1), List(1))
    var numCols: Int = 3
    var numRows: Int = 3
    var testState: BoardState = new BoardState(numRows, numCols, rowClues, colClues)
    //testState.solve().printBoard
	    
    //	5 by 5 Board
	//  X X X _ _ 3
	//  _ _ X _ _ 1
	//  _ _ X X X 3
    //  _ _ _ X X 2 
    //	_ X X X X 4
    //	1 1 3 3 3
    //	  1 1
    
    rowClues = Array(List(3), List(1), List(3), List(2), List(4))
    colClues = Array(List(1), List(1,1), List(3,1), List(3), List(3))
    numCols = 5
    numRows = 5
    testState = new BoardState(numRows, numCols, rowClues, colClues)
    //testState.solve().printBoard
    
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
    rowClues = Array(List(1,2), List(2), List(1), List(1), List(2),
        List(2,4), List(2,6), List(8), List(1,1), List(2,2))
    colClues = Array(List(2), List(3), List(1), List(2,1), List(5),
        List(4), List(4,1), List(1,5), List(1,2), List(2), List(2,1))
    numCols = 11
    numRows = 10
    testState = new BoardState(numRows, numCols, rowClues, colClues)
    testState.solve().printBoard
    
  }
}