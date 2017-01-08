package com.chess.pieces
import com.chess._

/**
  * Created by Karl-Mattias on 28.12.2016.
  */
case class King(colour: Colour) extends Piece{
  override val value: Int = 1000

  override def getMoves(square: Square, board: Board): Set[Move] = {

    val up: Square = Square(square.letter, square.number + 1)
    val down: Square = Square(square.letter, square.number - 1)
    val right: Square = Square((square.letter + 1).toChar, square.number)
    val left: Square = Square((square.letter - 1).toChar, square.number)
    val rightUp: Square = Square((square.letter + 1).toChar, square.number + 1)
    val rightDown: Square = Square((square.letter + 1).toChar, square.number - 1)
    val leftUp: Square = Square((square.letter - 1).toChar, square.number + 1)
    val leftDown: Square = Square((square.letter - 1).toChar, square.number - 1)

    val squares: Set[Square] = Set() + up + down + right + left + rightUp + rightDown + leftUp + leftDown

    var moves = for {
      toSquare <- squares
      if board.isLegal(Move(square, toSquare), colour)
    } yield Move(square, toSquare)

    if (!board.hasWhiteKingMoved && colour == White) {
      if (board.canWhiteQueensideCastle) moves += Move(square, Square('c', 1), queensideCastling = true)
      if (board.canWhiteKingsideCastle) moves += Move(square, Square('g', 1), kingsideCastling = true)
    }

    if (!board.hasBlackKingMoved && colour == Black) {
      if (board.canBlackQueensideCastle) moves += Move(square, Square('c', 8), queensideCastling = true)
      if (board.canBlackKingsideCastle) moves += Move(square, Square('g', 8), kingsideCastling = true)
    }

    moves
  }
}
