package com.chess

import com.chess.pieces.{King, Piece}

/**
  * Created by Karl-Mattias on 30.12.2016.
  *
  * A minimax algorithm with alpha beta pruning
  */
object AlphaBetaMinimax extends Player{

  private val maxDepth = 3

  override def nextMove(board: Board, colour: Colour): Move = {
    maxNode(board, colour, maxDepth, Int.MaxValue)._1
  }

  private def estimation(board: Map[Square, Piece], colour: Colour): Int = colour match {
    case White =>
      board.map({ case (square, piece) => if (piece.colour == colour) square.number + piece.value else -piece.value }).sum
    case Black =>
      board.map({ case (square, piece) => if (piece.colour == colour) 9 - square.number + piece.value else -piece.value }).sum
    case NoColour => throw new Error("You must define the colour of the player")
  }

  private def maxNode(board: Board, colour: Colour, depth: Int, beta: Int): (Move, Int) = {

    val possibleMoves: Set[Move] = getPossibleMoves(board, colour)

    if (possibleMoves.isEmpty) { // If stalemate
      val pieces = board.board.values.toSet
      val pointlessMove = Move(Square('a', 1), Square('a', 1))

      if (pieces.contains(King(colour)) && !pieces.contains(King(otherColour(colour)))) return (pointlessMove, 1000)
      else if (pieces.contains(King(otherColour(colour))) && !pieces.contains(King(colour))) return (pointlessMove, -1000)
      else return (pointlessMove, 0)
    }

    if (depth == 0) {
      val bestMoves: (Int, Set[Move]) = estimateMoves(possibleMoves, board, colour)
        .foldLeft(Int.MinValue -> Set[Move]())((best, current) => if (current._1 > best._1) current else best)
      (bestMoves._2.head, bestMoves._1)
    } else {
      var best = Int.MinValue
      var bestMove = possibleMoves.head

      for (move <- possibleMoves) {
        val newState = applyMove(move, board)
        val moveValue = minNode(new Board(newState), colour, depth - 1, best)._2
        if (best < moveValue) {
          best = moveValue
          bestMove = move
        }
        if (best > beta) return (bestMove, moveValue)
      }

      (bestMove, best)
    }
  }

  def maxEntry(entry1: (Int, Move), entry2: (Int, Move)): Move = {
    if (entry1._1 > entry2._1) entry1._2
    else entry2._2
  }

  private def minNode(board: Board, colour: Colour, depth: Int, alpha: Int): (Move, Int) = {

    val possibleMoves: Set[Move] = colour match {
      case White => getPossibleMoves(board, Black)
      case Black => getPossibleMoves(board, White)
      case NoColour => throw new Error("You must define the colour of the player")
    }

    if (depth == 0) {
      val bestMoves: (Int, Set[Move]) = estimateMoves(possibleMoves, board, colour)
        .foldLeft(Int.MaxValue -> Set[Move]())((best, current) => if (current._1 < best._1) current else best)
      (bestMoves._2.head, bestMoves._1)
    } else {
      var best = Int.MaxValue
      var bestMove = possibleMoves.head

      for (move <- possibleMoves) {
        val newState = applyMove(move, board)
        val moveValue = maxNode(new Board(newState), colour, depth - 1, best)._2
        if (best > moveValue) {
          best = moveValue
          bestMove = move
        }
        if (best < alpha) return (bestMove, best)
      }

      (bestMove, best)
    }
  }

  private def estimateMoves(moves: Set[Move], board: Board, colour: Colour): Map[Int, Set[Move]] = {
    moves.groupBy(move => estimation(applyMove(move, board), colour))
  }

  private def applyMove(move: Move, board: Board): Map[Square, Piece] = {
    val piece = board.board(move.from)
    board.board - move.from + (move.to -> piece)
  }

  private def getPossibleMoves(board: Board, colour: Colour): Set[Move] = {
    board.board.filter({case (_, piece) => piece.colour == colour })
      .flatMap({case (square, piece) =>  piece.getMoves(square, board)}).toSet
  }

  private def otherColour(colour: Colour): Colour = colour match {
    case White => Black
    case Black => White
    case NoColour => throw new Error("Colour must be defined")
  }
}
