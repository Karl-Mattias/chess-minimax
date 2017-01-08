package com.chess.pieces
import com.chess.{Board, Colour, Move, Square}

/**
  * Created by Karl-Mattias on 28.12.2016.
  */
case class Knight(colour: Colour) extends Piece{
  override val value: Int = 30

  override def getMoves(square: Square, board: Board): Set[Move] = {
    val fromLetter = square.letter
    val fromNumber = square.number

    val toSquares: Set[Square] = Set() + Square((fromLetter + 2).toChar, fromNumber + 1) +
      Square((fromLetter + 2).toChar, fromNumber - 1) + Square((fromLetter - 2).toChar, fromNumber - 1) +
      Square((fromLetter - 2).toChar, fromNumber + 1) + Square((fromLetter + 1).toChar, fromNumber + 2) +
      Square((fromLetter + 1).toChar, fromNumber - 2) + Square((fromLetter - 1).toChar, fromNumber + 2) +
      Square((fromLetter - 1).toChar, fromNumber - 2)

    for {
      toSquare <- toSquares
      if board.isLegal(Move(square, toSquare), colour)
    } yield Move(square, toSquare)
  }
}
