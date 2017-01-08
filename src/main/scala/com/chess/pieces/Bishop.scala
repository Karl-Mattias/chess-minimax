package com.chess.pieces
import com.chess.{Board, Colour, Move, Square}

/**
  * Created by Karl-Mattias on 28.12.2016.
  */
case class Bishop(colour: Colour) extends Piece{
  override val value: Int = 30

  override def getMoves(square: Square, board: Board): Set[Move] = {
    var moves: Set[Move] = Set()

    var toSquare = Square((square.letter + 1).toChar, square.number + 1)

    while (board.isLegal(Move(square, toSquare), colour) && board.isClearWay(square,toSquare)) {
      moves += Move(square, toSquare)
      toSquare = Square((toSquare.letter + 1).toChar, toSquare.number + 1)
    }

    toSquare = Square((square.letter - 1).toChar, square.number - 1)

    while (board.isLegal(Move(square, toSquare), colour) && board.isClearWay(square,toSquare)) {
      moves += Move(square, toSquare)
      toSquare = Square((toSquare.letter - 1).toChar, toSquare.number - 1)
    }

    toSquare = Square((square.letter + 1).toChar, square.number - 1)

    while (board.isLegal(Move(square, toSquare), colour) && board.isClearWay(square,toSquare)) {
      moves += Move(square, toSquare)
      toSquare = Square((toSquare.letter + 1).toChar, toSquare.number - 1)
    }

    toSquare = Square((square.letter - 1).toChar, square.number + 1)

    while (board.isLegal(Move(square, toSquare), colour) && board.isClearWay(square,toSquare)) {
      moves += Move(square, toSquare)
      toSquare = Square((toSquare.letter - 1).toChar, toSquare.number + 1)
    }

//    println(moves)
    moves - Move(square, square)
  }
}
