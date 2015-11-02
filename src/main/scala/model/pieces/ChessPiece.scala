package main.model.pieces

import main.model.{Move, Colors, Square, ChessBoard}

import main.extensions.ImplicitVector._

/**
 * Created by kdoherty on 10/23/15.
 */
abstract class ChessPiece {

  val color: Colors.Value
  val symbol: Char
  val isKing = false
  val isPawn = false
  val isRook = false

  lazy val isWhite = color == Colors.WHITE
  lazy val isBlack = !isWhite
  lazy val oppColor = Colors.opp(color)
  lazy val getSymbol: Char = if (isWhite) symbol else Character.toUpperCase(symbol)

  def getAttackedSquares(board: ChessBoard, sq: Square): Vector[Square]
  def makeMove(move: Move): ChessBoard

  def getMoves(board: ChessBoard, sq: Square): Vector[Move] = {
    getAttackedSquares(board, sq)
      .map(new Move(board, sq, _))
      .filterNot(_.made.kingInCheck(color))
  }

  def isLegal(move: Move): Boolean = {
    // TODO don't need to get all moves
    getMoves(move.board, move.from).contains(move)
  }

  def unblockedSquares(board: ChessBoard, sqs: Vector[Square]): Vector[Square] = {
    if (sqs.isEmpty) {
      return sqs
    }

    val unblocked = sqs.takeWhileInclusive(board.isEmpty)

    if (board.containsPiece(unblocked.last, color)) {
      return unblocked.dropRight(1)
    }

    unblocked
  }

  override def toString = (if (isWhite) symbol else Character.toUpperCase(symbol)).toString

}
