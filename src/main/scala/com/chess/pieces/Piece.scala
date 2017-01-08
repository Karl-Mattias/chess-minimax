package com.chess.pieces

import com.chess._

/**
  * Created by Karl-Mattias on 22.12.2016.
  */

trait Piece {
  val colour: Colour
  val value: Int
  def getMoves(square: Square, board: Board): Set[Move]
}

object Empty extends Piece {
  override val colour = NoColour
  override val value: Int = 0
  override def getMoves(square: Square, board: Board): Set[Move] = Set()
}