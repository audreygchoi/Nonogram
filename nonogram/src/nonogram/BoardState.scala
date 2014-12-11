package nonogram

import java.util.Stack
import scala.util.control.Breaks._

object Mark extends Enumeration {
  val unknown, blank, filled = Value
}

class Board(numrows: Int, numcols: Int) {
  //at a given location, 
  //0 is going to be "unknown"
  //1 is going to be "blank"
  //2 is going to be "marked" (the value indicated by clues)
	var board = Array.ofDim[Mark.Value](numrows, numcols)
	
	for (i <- 0 until numrows){
	  for (j <- 0 until numcols) {
	    board(i)(j) = Mark.unknown
	  }
	}
	
	def getMark(row: Int, col: Int): Mark.Value = {
	  try {
	    board(row)(col)
	  } catch {
	    case e: Exception => {
	      println("Illegal getMark call")
	      return Mark.unknown 
	    }
	  }
	}
	
	def makeMark(row: Int, col: Int, mark: Mark.Value) : Boolean = {
	  try { 
		  if ( board(row)(col) == Mark.unknown ) {
		    board(row)(col) = mark
		    return true
		  }
		  //if this returns false something has gone wrong
		  println("Tried to make a mark in a known space")
		  false
	  } catch {
	    case e: Exception => {
	    	println("Out of bounds call to makeMark")
	    	return false
	    }
	  }
	}
	
	def printBoard = {
	  for (i <- 0 until numrows){
		  for (j <- 0 until numcols) {
		    board(i)(j) match {
		      case Mark.unknown =>
		        print("?")
		      case Mark.blank =>
		        print("_")
		      case Mark.filled =>
		        print("X")
		    }
		  }
		  println()
		}
	}
	
	def isFull: Boolean = {
	  for (i <- 0 until numrows) {
		  for (j <- 0 until numcols) {
			  if(board(i)(j) == Mark.unknown) false
		  }
	  }
	  true
	}
}

class BoardState(numrows: Int, numcols: Int, rowclues: Array[Stack[Int]], colclues: Array[Stack[Int]]){
	val numRows = numrows
	val numCols = numcols
	//right to left top to bottom
	val rowClues: Array[Stack[Int]] = rowclues
	val colClues: Array[Stack[Int]] = colclues
	val board = new Board(numrows, numcols)
	
	def hasContradictions(board:Board):Boolean = {
	  for(j <- 0 until numCols) {
	    var listOfClues = colClues(j)
	    var currentClue = listOfClues.pop
        var counter = 0
	    for(i <- 0 until numRows) {
	      board.getMark(i,j) match {
		    case Mark.filled =>
		      {
		        counter += 1
		        if(counter > currentClue) return true //this has a contradiction
		      }
		      case Mark.blank => {
		        if(counter == currentClue) {
		          currentClue = listOfClues.pop
		          counter = 0
		        }
		        else if(counter < currentClue && counter != 0) return true //this has a contradiction
		        //don't have to deal with the counter > currentClue case, because that is taken care of in case Mark.filled
		      }
		      case Mark.unknown =>
		        break
		        //we've reached the end of our guesses for this column
		        //if we haven't yet found a contradiction in this column, then there's none to find
		    }
	    }
	  }
	  return false
	}
	
//	def solve(board:Board):Board = {
//	  if (hasContradictions(board)) return board
//	  if (board.isFull) return board
//	  for (guess <- generateGuesses(board)) {
//	    solve(guess)
//	  }
//	}
}

object Main {
  def main(args: Array[String]) {
    val testBoard = new Board(5,5)
    testBoard.makeMark(0, 2, Mark.filled )
    testBoard.printBoard
  }
}