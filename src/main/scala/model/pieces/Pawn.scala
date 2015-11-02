package main.model.pieces

import main.model.{ChessBoard, Colors, Move, Square}

/**
 * Created by kdoherty on 10/23/15.
 */
case class Pawn(override val color: Colors.Value) extends ChessPiece {
  override val isPawn = true

  override val symbol: Char = 'p'

  override def makeMove(move: Move): ChessBoard = {
    val board = move.board
    val to = move.to
    val from = move.from

    if (to.row == ChessBoard.homeRow(oppColor)) {
      return board.remove(from).setPiece(to, Some(move.finalPiece.get))
    }

    val resultBoard = board.movePieceUpdatedBoard(from, to)
    if (board.enPassantSqOpt.isDefined && board.enPassantSqOpt.get == to) {
      return resultBoard.remove(ChessBoard.behind(to, color).get)
    }

    if (from.verticalDist(to) == 2) {
      val isOppColoredPawn = (piece: ChessPiece) => piece.color == oppColor && piece.isPawn
      val leftOpt = ChessBoard.left(to, color).fold(Option.empty[ChessPiece])(board.getPiece)
      lazy val rightOpt = ChessBoard.right(to, color).fold(Option.empty[ChessPiece])(board.getPiece)
      if ((leftOpt.isDefined && isOppColoredPawn(leftOpt.get)) ||
        (rightOpt.isDefined && isOppColoredPawn(rightOpt.get))) {
        return resultBoard.copy(enPassantSqOpt = Some(ChessBoard.behind(to, color).get))
      }
    }

    resultBoard
  }

  override def getMoves(board: ChessBoard, sq: Square): Vector[Move] = {

    def isPromoting(board: ChessBoard, to: Square): Boolean = {
      to.row == ChessBoard.finalRow(color)
    }

    def getPromotionMoves(board: ChessBoard, from: Square, to: Square): Vector[Move] = {
      Vector(
        new Move(board, from, to, Some(new Queen(color))),
        new Move(board, from, to, Some(new Rook(color))),
        new Move(board, from, to, Some(new Bishop(color))),
        new Move(board, from, to, Some(new Knight(color))))
    }

    def squaresToMoves(sqs: Vector[Square]): Vector[Move] = {
      sqs.foldLeft(Vector[Move]()) { (acc, to) =>
        if (isPromoting(board, to)) {
          acc ++ getPromotionMoves(board, sq, to)
        } else {
          acc :+ new Move(board, sq, to)
        }
      }.filterNot(_.made.kingInCheck(color))
    }

    val ahead = ChessBoard.ahead(sq, color).get
    val leftOpt = ChessBoard.left(ahead, color)
    val rightOpt = ChessBoard.right(ahead, color)
    val diagAheadSqs = Vector(leftOpt, rightOpt)
    val validDiagSqs = diagAheadSqs
      .filter(sqOpt => sqOpt.isDefined && board.containsPiece(sqOpt.get, oppColor))
      .map(_.get)


    if (board.isEmpty(ahead)) {
      // Make sure when on 7th rank and square ahead is empty this doesn't break
      lazy val twoAhead = ChessBoard.ahead(ahead, color).get
      val canMoveTwo = ChessBoard.pawnHomeRow(color) == sq.row && board.isEmpty(twoAhead)
      if (canMoveTwo) {
        squaresToMoves(validDiagSqs :+ ahead :+ twoAhead)
      } else {
        squaresToMoves(validDiagSqs :+ ahead)
      }
    } else {
      squaresToMoves(validDiagSqs)
    }
  }

  override def getAttackedSquares(board: ChessBoard, sq: Square): Vector[Square] = {
    val ahead = ChessBoard.ahead(sq, color).get
    val leftOpt = ChessBoard.left(ahead, color)
    val rightOpt = ChessBoard.right(ahead, color)
    if (leftOpt.isEmpty) {
      Vector(rightOpt.get)
    } else if (rightOpt.isEmpty) {
      Vector(leftOpt.get)
    } else {
      Vector(leftOpt.get, rightOpt.get)
    }
  }
}