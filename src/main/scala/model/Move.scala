package main.model

import main.model.pieces._

/**
 * Created by kdoherty on 10/24/15.
 */
case class Move(board: ChessBoard, from: Square, to: Square, finalPiece: Option[ChessPiece] = None) {

  lazy val piece: ChessPiece = board.getPiece(from).get
  lazy val made: ChessBoard = piece.makeMove(this)

  lazy val isPromotion: Boolean = piece.isPawn && ChessBoard.finalRow(piece.color) == to.row
  private lazy val isCastle: Boolean = piece.isKing && from.horizontalDist(to) == 2
  private lazy val isCheck: Boolean = made.kingInCheck(piece.oppColor)
  private lazy val isTaking: Boolean = board.containsPiece(to, piece.oppColor)

  lazy val isLegal: Boolean = piece.isLegal(this)

  override def toString: String = {

    def addPlusIfCheck(moveStr: String): String = if (isCheck) moveStr + "+" else moveStr

    if (isCastle) {
      val moveStr = if (to.col == 6) "O-O" else "O-O-O"
      return addPlusIfCheck(moveStr)
    }

    val takingX = if (isTaking) "x" else ""

    if (isPromotion) {
      val moveStr = piece.toString + takingX + to.toString + finalPiece.get.symbol
      return addPlusIfCheck(moveStr)
    }

    addPlusIfCheck(piece.toString + takingX + to.toString)
  }

}
