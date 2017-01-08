package com.chess

import com.chess.pieces._

import scala.io.StdIn

/**
  * Created by Karl-Mattias on 30.12.2016.
  */
object ManualControl extends Player{
  override def nextMove(board: Board, colour: Colour): Move = {
    while (true){
      print("Enter your move: ")
      Console.flush()
      try {
        val in: String = StdIn.readLine()
        val fromString: String = in.substring(0,2)
        if (fromString.equals("00")) {
          if (in.length == 2) colour match {
            case White => return checkThenReturn(board, Move(Square('e', 1), Square('g', 1), kingsideCastling = true), colour)
            case Black => return checkThenReturn(board, Move(Square('e', 8), Square('g', 8), kingsideCastling = true), colour)
            case NoColour => throw new Error("You must define the colour of the player")
          } else if (in.charAt(2) == '0') colour match {
            case White => return checkThenReturn(board, Move(Square('e', 1), Square('c', 1), queensideCastling = true), colour)
            case Black => return checkThenReturn(board, Move(Square('e', 8), Square('c', 8), queensideCastling = true), colour)
            case NoColour => throw new Error("You must define the colour of the player")
          }
        }
        val toString: String = in.substring(in.length - 2, in.length)

        val from: Square = Square(fromString.charAt(0), fromString.substring(1).toInt)
        val to: Square = Square(toString.charAt(0), toString.substring(1).toInt)

        var move = Move(from, to)

        if (board.board(from) == Pawn(colour) && (to.number == 1 || to.number == 8)) {
          print("What do you want your pawn to be prompted to? q,n,r or b: ")
          val promoteTo: Char = StdIn.readChar()
          promoteTo match {
            case 'q' => move = Move(from, to, isPromotion = true, promoteTo = Queen(colour))
            case 'n' => move = Move(from, to, isPromotion = true, promoteTo = Knight(colour))
            case 'r' => move = Move(from, to, isPromotion = true, promoteTo = Rook(colour))
            case 'b' => move = Move(from, to, isPromotion = true, promoteTo = Bishop(colour))
          }
        }

        return checkThenReturn(board, move, colour)
      } catch {
        case e: Exception => e.printStackTrace()
        case e: Error => e.printStackTrace()
      }
    }
    throw new Error("Should never get here")
  }

  def checkThenReturn(board: Board, move: Move, colour: Colour): Move = {
    if (!board.isLegal(move, colour) || !board.board(move.from).getMoves(move.from, board).contains(move)) throw new Error("Illegal move")
    move
  }
}
