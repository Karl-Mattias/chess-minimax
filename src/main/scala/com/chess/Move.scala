package com.chess

import com.chess.pieces.{Piece, Queen}

/**
  * Created by Karl-Mattias on 22.12.2016.
  */

case class Square (letter: Char, number: Int)

case class Move (from: Square, to: Square,
                 isPromotion: Boolean = false, promoteTo: Piece = Queen(NoColour),
                 kingsideCastling: Boolean = false, queensideCastling: Boolean = false)
