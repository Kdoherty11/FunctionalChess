package main.model.pieces

import main.model.{Move, Colors, Square, ChessBoard}

/**
 * Created by kdoherty on 10/23/15.
 */
case class Queen(override val color: Colors.Value) extends ChessPiece {

  override val symbol: Char = 'q'

  override def getAttackedSquares(board: ChessBoard, sq: Square): Vector[Square] = {
    (ChessBoard.crossFrom(sq) ++ ChessBoard.diagonalFrom(sq)).flatMap(unblockedSquares(board, _))
  }

  override def makeMove(move: Move): ChessBoard = move.board.movePieceUpdatedBoard(move.from, move.to)
}