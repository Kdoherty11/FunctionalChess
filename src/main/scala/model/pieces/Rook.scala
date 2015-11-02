package main.model.pieces

import main.model.{Move, Colors, Square, ChessBoard}

/**
 * Created by kdoherty on 10/23/15.
 */
case class Rook(override val color: Colors.Value, hasMoved: Boolean = false) extends ChessPiece {

  override val isRook = true
  override val symbol: Char = 'r'

  override def getAttackedSquares(board: ChessBoard, sq: Square): Vector[Square] = {
    ChessBoard.crossFrom(sq).flatMap(unblockedSquares(board, _))
  }

  override def makeMove(move: Move): ChessBoard = {
    val board = move.board

    def movePieceFn(from: Square, to: Square) = board.remove(from).setPiece(to, Some(new Rook(color, true)))

    board.movePieceUpdatedBoard(move.from, move.to, movePieceFn)
  }
}