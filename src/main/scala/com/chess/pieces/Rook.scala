package com.chess.pieces
import com.chess.{Board, Colour, Move, Square}

/**
  * Created by Karl-Mattias on 28.12.2016.
  */
case class Rook(colour: Colour) extends Piece{
  override val value: Int = 50

  override def getMoves(square: Square, board: Board): Set[Move] = {
    var toSquares: Set[Square] = Set()

    val initialLetter = square.letter
    val initialNumber = square.number

    for (letter <- 'a' to 'h') {
      toSquares = toSquares + Square(letter, initialNumber)
    }
    for (number <- 1 to 8) {
      toSquares = toSquares + Square(initialLetter, number)
    }

    for {
      toSquare <- toSquares
      if board.isLegal(Move(square, toSquare), colour) && board.isClearWay(square, toSquare)
    } yield Move(square, toSquare)
  }
}
