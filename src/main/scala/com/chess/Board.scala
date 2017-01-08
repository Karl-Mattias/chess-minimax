package com.chess

import com.chess.pieces._

/**
  * Created by Karl-Mattias on 22.12.2016.
  */
class Board(state: Map[Square, Piece]) {

  def this() = this(Map())

  var board: Map[Square, Piece] = state withDefaultValue Empty

  def isFree(square: Square): Boolean =
    square.letter <= 'h' && square.number <= 8 && board(square) == Empty

  def isOfColour(square: Square, colour: Colour): Boolean =
    square.letter <= 'h' && square.number <= 8 && board(square).colour == colour

  def isLegal(move: Move, yourColour: Colour): Boolean =
    move.to.letter >= 'a' && move.to.letter <= 'h' && move.to.number <= 8 && move.to.number >= 1 &&
      board(move.to).colour != yourColour && board(move.from).colour == yourColour

  def isClearWay(fromSquare: Square, toSquare: Square): Boolean = {
    val newLetter = if (fromSquare.letter < toSquare.letter) (fromSquare.letter + 1).toChar else
                    if (fromSquare.letter == toSquare.letter) fromSquare.letter else
                    (fromSquare.letter - 1).toChar

    val newNumber = if (fromSquare.number < toSquare.number) fromSquare.number + 1 else
                    if (fromSquare.number == toSquare.number) fromSquare.number else
                    fromSquare.number - 1

    val newSquare = Square(newLetter, newNumber)
    if (toSquare == newSquare) {
      return true
    }
    board(newSquare) == Empty && isClearWay(newSquare, toSquare)
  }

  var hasBlackKingMoved = false
  var hasBlackQueensideRookMoved = false
  var hasBlackKingsideRookMoved = false
  var hasWhiteKingMoved = false
  var hasWhiteQueensideRookMoved = false
  var hasWhiteKingsideRookMoved = false

  // TODO: Check that the king is not under check while castling
  def canWhiteQueensideCastle: Boolean =
    !hasWhiteQueensideRookMoved && !hasWhiteKingMoved &&
      board(Square('b',1)) == Empty && board(Square('c',1)) == Empty && board(Square('d',1)) == Empty


  def canBlackQueensideCastle: Boolean =
    !hasBlackQueensideRookMoved && !hasBlackKingMoved &&
      board(Square('b',8)) == Empty && board(Square('c',8)) == Empty && board(Square('d',8)) == Empty


  def canWhiteKingsideCastle: Boolean =
    !hasWhiteKingsideRookMoved && !hasWhiteKingMoved &&
      board(Square('f',1)) == Empty && board(Square('g',1)) == Empty

  def canBlackKingsideCastle: Boolean =
    !hasBlackKingsideRookMoved && !hasBlackKingMoved &&
      board(Square('f',8)) == Empty && board(Square('g',8)) == Empty



  def initialize(): Unit = {
    for (i <- 'a' to 'h') {
      board += Square(i, 2) -> Pawn(White)
      board += Square(i, 7) -> Pawn(Black)
    }

    board += Square('a', 1) -> Rook(White)
    board += Square('h', 1) -> Rook(White)
    board += Square('a', 8) -> Rook(Black)
    board += Square('h', 8) -> Rook(Black)

    board += Square('b', 1) -> Knight(White)
    board += Square('g', 1) -> Knight(White)
    board += Square('b', 8) -> Knight(Black)
    board += Square('g', 8) -> Knight(Black)

    board += Square('c', 1) -> Bishop(White)
    board += Square('f', 1) -> Bishop(White)
    board += Square('c', 8) -> Bishop(Black)
    board += Square('f', 8) -> Bishop(Black)

    board += Square('d', 1) -> Queen(White)
    board += Square('e', 1) -> King(White)
    board += Square('d', 8) -> Queen(Black)
    board += Square('e', 8) -> King(Black)
  }

  def step(move: Move): Unit = {
    val piece: Piece = board(move.from)
    board -= move.from
    board += move.to -> piece

    if (move.isPromotion && (piece == Pawn(Black) && move.to.number == 1 || piece == Pawn(White) && move.to.number == 8)) {
      move.promoteTo match {
        case Queen(_)  => board += move.to -> Queen(piece.colour)
        case Knight(_) => board += move.to -> Knight(piece.colour)
        case Rook(_)   => board += move.to -> Rook(piece.colour)
        case Bishop(_) => board += move.to -> Bishop(piece.colour)
      }
    }

    if (!hasWhiteKingMoved && piece == King(White)) hasWhiteKingMoved = true
    if (!hasBlackKingMoved && piece == King(Black)) hasBlackKingMoved = true

    if (!hasBlackKingsideRookMoved && piece == Rook(Black) && move.from.letter == 'h') hasBlackKingsideRookMoved = true
    if (!hasWhiteKingsideRookMoved && piece == Rook(White) && move.from.letter == 'h') hasWhiteKingsideRookMoved = true
    if (!hasBlackQueensideRookMoved && piece == Rook(Black) && move.from.letter == 'a') hasBlackQueensideRookMoved = true
    if (!hasWhiteQueensideRookMoved && piece == Rook(White) && move.from.letter == 'a') hasWhiteQueensideRookMoved = true

    if (move.kingsideCastling) step(Move(Square('h', move.from.number), Square('f', move.from.number)))
    if (move.queensideCastling) step(Move(Square('a', move.from.number), Square('d', move.from.number)))
  }

  def isGameOver: Boolean =
    !board.values.exists(_ == King(White)) || !board.values.exists(_ == King(Black))

  override def toString: String = {
    var output: String = "     a b c d e f g h\n\n"
    for (i <- 8 to 1 by -1) {
      output += "     - - - - - - - - \n" + i + "   "
      for (j <- 'a' to 'h') {
        output += "|"
         board(Square(j, i)) match {
           case Empty => output += " "
           case Pawn(White) => output += "P"
           case Pawn(Black) => output += "p"
           case King(White) => output += "K"
           case King(Black) => output += "k"
           case Knight(Black) => output += "n"
           case Knight(White) => output += "N"
           case Bishop(Black) => output += "b"
           case Bishop(White) => output += "B"
           case Rook(White) => output += "R"
           case Rook(Black) => output += "r"
           case Queen(White) => output += "Q"
           case Queen(Black) => output += "q"
        }
      }
      output += "|\n"
    }
    output + "     - - - - - - - - "
  }
}
