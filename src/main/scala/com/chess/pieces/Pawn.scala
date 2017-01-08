package com.chess.pieces

import com.chess._

/**
  * Created by Karl-Mattias on 22.12.2016.
  */
case class Pawn(colour: Colour) extends Piece {
  var hasMoved = false
  override val value: Int = 10

  override def getMoves(square: Square, board: Board): Set[Move] = {
    var moves: Set[Move] = Set[Move]()

    colour match {
      case White =>
        hasMoved = !(square.number == 2)

        val oneForward = Square(square.letter, square.number + 1)
        val twoForward = Square(square.letter, square.number + 2)
        if (board.isFree(oneForward)){

          if (oneForward.number == 8) {
            moves += Move(square, oneForward, isPromotion = true, promoteTo = Queen(White))
            moves += Move(square, oneForward, isPromotion = true, promoteTo = Knight(White))
            moves += Move(square, oneForward, isPromotion = true, promoteTo = Rook(White))
            moves += Move(square, oneForward, isPromotion = true, promoteTo = Bishop(White))
          }
          else moves += Move(square, oneForward)

          if (! hasMoved && board.isFree(twoForward))
            moves += Move(square, twoForward)
        }

        val attackLeft = Square((square.letter - 1).toChar, square.number + 1)
        if (board.isOfColour(attackLeft, Black)) {
          if (attackLeft.number == 8) {
            moves += Move(square, attackLeft, isPromotion = true, promoteTo = Queen(White))
            moves += Move(square, attackLeft, isPromotion = true, promoteTo = Knight(White))
            moves += Move(square, attackLeft, isPromotion = true, promoteTo = Rook(White))
            moves += Move(square, attackLeft, isPromotion = true, promoteTo = Bishop(White))
          }
          else moves += Move(square, attackLeft)
        }

        val attackRight = Square((square.letter + 1).toChar, square.number + 1)
        if (board.isOfColour(attackRight, Black)){
          if (attackRight.number == 8) {
            moves += Move(square, attackRight, isPromotion = true, promoteTo = Queen(White))
            moves += Move(square, attackRight, isPromotion = true, promoteTo = Knight(White))
            moves += Move(square, attackRight, isPromotion = true, promoteTo = Rook(White))
            moves += Move(square, attackRight, isPromotion = true, promoteTo = Bishop(White))
          }
          else moves += Move(square, attackRight)
        }

      case Black =>
        hasMoved = !(square.number == 7)

        val oneForwards = Square(square.letter, square.number - 1)
        val twoForwards = Square(square.letter, square.number - 2)
        if (board.isFree(oneForwards)){

          if (oneForwards.number == 1) {
            moves += Move(square, oneForwards, isPromotion = true, promoteTo = Queen(Black))
            moves += Move(square, oneForwards, isPromotion = true, promoteTo = Knight(Black))
            moves += Move(square, oneForwards, isPromotion = true, promoteTo = Rook(Black))
            moves += Move(square, oneForwards, isPromotion = true, promoteTo = Bishop(Black))
          }
          else moves += Move(square, oneForwards)

          if (! hasMoved && board.isFree(twoForwards))
            moves += Move(square, twoForwards)
        }

        val attackLeft = Square((square.letter + 1).toChar, square.number - 1)
        if (board.isOfColour(attackLeft, White)){
          if (attackLeft.number == 1) {
            moves += Move(square, attackLeft, isPromotion = true, promoteTo = Queen(Black))
            moves += Move(square, attackLeft, isPromotion = true, promoteTo = Knight(Black))
            moves += Move(square, attackLeft, isPromotion = true, promoteTo = Rook(Black))
            moves += Move(square, attackLeft, isPromotion = true, promoteTo = Bishop(Black))
          }
          else moves += Move(square, attackLeft)
        }

        val attackRight = Square((square.letter - 1).toChar, square.number - 1)
        if (board.isOfColour(attackRight, White)){
          if (attackRight.number == 1) {
            moves += Move(square, attackRight, isPromotion = true, promoteTo = Queen(Black))
            moves += Move(square, attackRight, isPromotion = true, promoteTo = Knight(Black))
            moves += Move(square, attackRight, isPromotion = true, promoteTo = Rook(Black))
            moves += Move(square, attackRight, isPromotion = true, promoteTo = Bishop(Black))
          }
          else moves += Move(square, attackRight)
        }
      case NoColour => throw new Error("You must define the colour of the pawn")
    }
    moves
  }
}
