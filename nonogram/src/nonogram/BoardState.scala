package nonogram

import scala.collection.mutable.Stack
import scala.io.Source
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

class BoardState(numRows: Int, numCols: Int, rowClues: Array[List[Int]], colClues: Array[List[Int]]){
	val board = new Board(numRows, numCols)
	
	def hasContradictions(b:Board):Boolean = {
	  for(j <- 0 until numCols) {
	    var listOfClues = colClues(j)
	    var currentClue = 0
	    listOfClues match {
	      case h::t => {
	        currentClue = h
	        listOfClues = t
	      }
	      case nil => {}
	    }
        var counter = 0
        var remainingSlots = 0
	    for(i <- 0 until numRows) {
	      b.getMark(i,j) match {
		    case Mark.filled =>
		      {
		        counter += 1
		        if(counter > currentClue) return true //this has a contradiction
		      }
		      case Mark.blank => {
		        if(counter == currentClue) {
		        	currentClue = 0
		        	listOfClues match {
		        		case h::t => {
		        			currentClue = h
		        			listOfClues = t
		        		}
		        		case nil => {}
		        	}
		        	counter = 0
		        }
		        else if(counter < currentClue && counter != 0) return true //this has a contradiction
		        //don't have to deal with the counter > currentClue case, because that is taken care of in case Mark.filled
		      }
		      case Mark.unknown =>
		        if(remainingSlots == 0) remainingSlots = numRows - i
		        //we've reached the end of our guesses for this column
		        //if we haven't yet found a contradiction in this column, then there's none to find
		    }
	    }
        //if we have clues left over at the end, sum them up and check them against our remainingSlots
        if(!listOfClues.isEmpty) {
          var sum: Int = 0
          for (clue <- listOfClues) {
            sum = sum + 1 + clue
          }
          sum -= 1
          if(sum > remainingSlots) return true
        }
	  }
	  return false
	}
	
	//needs testing
	def isLastGuessRow(row:Array[Mark.Value], cluesList:List[Int]): Boolean = {
	  var lastIndex = numCols - 1
	  var clueList = cluesList.reverse
	  for(clue <- clueList) {
	    for(i <- 0 until clue) {
	      if(row(lastIndex) != Mark.filled) return false
	      lastIndex -= 1
	    }
	    if(lastIndex < numCols) {
	      if(row(lastIndex) != Mark.blank) return false 
	      lastIndex -= 1
	    }
	  }
	  return true
	}
	
	def firstGuess(cluesList:List[Int]): Array[Mark.Value] = {
	  var guessRow:Array[Mark.Value] = new Array[Mark.Value](numCols)
	  var lastIndex = 0
	  for(clue <- cluesList) {
	    for(i <- 0 until clue) {
	      guessRow(lastIndex) = Mark.filled
	      lastIndex += 1
	    }
	    if(lastIndex < numCols) {
	      guessRow(lastIndex) = Mark.blank 
	      lastIndex += 1
	    }
	  }
	  for(i <- lastIndex until numCols) {
	    guessRow(i) = Mark.blank
	  }
	  return guessRow
	}
	
	
	def nextGuessRow(board:Board, rowNum:Int):Board = {
	  //if we're over, something terribly wrong has happened
	  if(rowNum >= numRows) return new Board(numRows, numCols)
	  
	  var row:Array[Mark.Value] = board.board(rowNum)
	  var cluesList:List[Int] = rowClues(rowNum)
	  if(isLastGuessRow(row, cluesList)) {
	    board.board(rowNum) = firstGuess(cluesList)
	    nextGuessRow(board, rowNum + 1)
	  } else {
	    //TODO
	    //go clue by clue? treat them in the same 
	    //doincrement
	  }
	  return board
	}
	
	def nextGuess(board:Board):Board = {
	  return nextGuessRow(board, 0)
	}
	
	def solve(board:Board):Board = {
	  if (!hasContradictions(board)) return board
	  solve(nextGuess(board))
	}
}

