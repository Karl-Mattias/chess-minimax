package com.chess

/**
  * Created by Karl-Mattias on 30.12.2016.
  */
object Game {

  val whitePlayer: Player = ManualControl
  val blackPlayer: Player = AlphaBetaMinimax

  def main(args: Array[String]): Unit = {
    println("\nWhen manually controlling one player then the moves are in the form of b1c3 for example.")
    println("Kingside castling command is 00 and queenside is 000")
    val board: Board = new Board()
    board.initialize()
    do {
      println(board)
      val whiteMove = whitePlayer.nextMove(board, White)
      println(whiteMove)
      if (!board.isLegal(whiteMove, White)) throw new Error("Illegal move")
      board.step(whiteMove)

      println(board)
      val blackMove = blackPlayer.nextMove(board, Black)
      println(blackMove)
      if (!board.isLegal(blackMove, Black)) throw new Error("Illegal move")
      board.step(blackMove)
    } while (! board.isGameOver)
  }
}
