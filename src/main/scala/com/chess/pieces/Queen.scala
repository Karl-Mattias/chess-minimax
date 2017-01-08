package com.chess.pieces
import com.chess.{Board, Colour, Move, Square}

/**
  * Created by Karl-Mattias on 28.12.2016.
  */
case class Queen(colour: Colour) extends Piece{
  override val value: Int = 90

  override def getMoves(square: Square, board: Board): Set[Move] =
    Rook(colour).getMoves(square, board) ++ Bishop(colour).getMoves(square, board)
}
