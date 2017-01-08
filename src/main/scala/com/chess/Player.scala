package com.chess


/**
  * Created by Karl-Mattias on 30.12.2016.
  */
trait Player {
  def nextMove(board: Board, colour: Colour): Move
}
