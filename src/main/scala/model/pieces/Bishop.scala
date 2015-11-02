package main.model.pieces

import main.model.{Move, Colors, Square, ChessBoard}

/**
 * Created by kdoherty on 10/23/15.
 */
case class Bishop(override val color: Colors.Value) extends ChessPiece {

  override val symbol: Char = 'b'

  override def makeMove(move: Move): ChessBoard = move.board.movePieceUpdatedBoard(move.from, move.to)

  override def getAttackedSquares(board: ChessBoard, sq: Square): Vector[Square] = {
    ChessBoard.diagonalFrom(sq).flatMap(unblockedSquares(board, _))
  }

}