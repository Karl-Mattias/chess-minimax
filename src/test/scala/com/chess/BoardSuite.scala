package com.chess

import org.scalatest.FunSuite
/**
  * Created by Karl-Mattias on 28.12.2016.
  */
class BoardSuite extends FunSuite {

  val board: Board = new Board
  board.initialize()

  test("1) isClearWay"){
    assert(!board.isClearWay(Square('a', 1), Square('h', 8)))
    assert(board.isClearWay(Square('a', 4), Square('h', 4)))
    assert(board.isClearWay(Square('a', 2), Square('f', 7)))
  }
}
