package main.model.pieces

import main.model.{ChessBoard, Colors, Move, Square}

/**
 * Created by kdoherty on 10/23/15.
 */
case class King(override val color: Colors.Value, hasMoved: Boolean = false) extends ChessPiece {

  override val symbol: Char = 'k'

  override def getMoves(board: ChessBoard, sq: Square): Vector[Move] = {
    val kingStartSq = if (isWhite) ChessBoard.WhiteKingStartSquare else ChessBoard.BlackKingStartSquare

    val kingCanCastle = sq == kingStartSq && !hasMoved && !board.kingInCheck(color)

    def isEligibleRook(rookSq: Square): Boolean = {
      val pieceOpt = board.getPiece(rookSq)
      if (pieceOpt.isDefined) {
        val piece = pieceOpt.get
        return piece.isRook && piece.color == color && !piece.asInstanceOf[Rook].hasMoved
      }

      false
    }

    val canCastleShort = {
      lazy val btwnSqs = (1 to 2).map(i => new Square(sq.row, sq.col + i))
      lazy val hasSpace = btwnSqs.forall(board.isEmpty)
      lazy val isCutOff = btwnSqs.exists(board.isAttackedBy(_, oppColor))
      lazy val hasEligibleRook = isEligibleRook(new Square(sq.row, ChessBoard.MaxColIndex))

      kingCanCastle && hasSpace && !isCutOff && hasEligibleRook
    }

    val canCastleLong = {
      lazy val btwnSqs = (1 to 3).map(i => new Square(sq.row, sq.col - i))
      lazy val hasSpace = btwnSqs.forall(board.isEmpty)
      lazy val isCutOff = btwnSqs.dropRight(1).exists(board.isAttackedBy(_, oppColor))
      lazy val hasEligibleRook = isEligibleRook(new Square(sq.row, 0))

      kingCanCastle && hasSpace && !isCutOff && hasEligibleRook
    }

    lazy val shortCastleMove: Move = new Move(board, sq, new Square(sq.row, sq.col + 2))
    lazy val longCastleMove: Move = new Move(board, sq, new Square(sq.row, sq.col - 2))

    val posMoves = getAttackedSquares(board, sq)
      .map(new Move(board, sq, _))
      .filterNot(_.made.kingInCheck(color))

    if (canCastleShort && canCastleLong) {
      posMoves :+ shortCastleMove :+ longCastleMove
    } else if (canCastleShort) {
      posMoves :+ shortCastleMove
    } else if (canCastleLong) {
      posMoves :+ longCastleMove
    } else {
      posMoves
    }
  }

  override def getAttackedSquares(board: ChessBoard, sq: Square): Vector[Square] = {
    ChessBoard.neighbors(sq).filterNot(board.containsPiece(_, color))
  }

  override def makeMove(move: Move): ChessBoard = {
    val board = move.board
    val to = move.to
    val from = move.from

    val whiteKingSq = if (isWhite) to else board.whiteKingSq
    val blackKingSq = if (isBlack) to else board.blackKingSq

    def movePieceFn(from: Square, to: Square) = board.remove(from).setPiece(to, Some(new King(color, true)))

    val resultBoard = board.movePieceUpdatedBoard(from, to, movePieceFn)
      .copy(whiteKingSq = whiteKingSq, blackKingSq = blackKingSq)

    if (from.horizontalDist(to) == 2) {
      val fromRookSq = if (to.col == 2) new Square(to.row, 0) else new Square(to.row, 7)
      val toRookSq = if (to.col == 2) new Square(to.row, 3) else new Square(to.row, 5)
      val whiteCastled = if (isWhite) true else board.whiteCastled
      val blackCastled = if (isBlack) true else board.blackCastled

      // Castling
      return resultBoard.movePiece(fromRookSq, toRookSq).copy(
        whiteCastled = whiteCastled, blackCastled = blackCastled)
    }

    resultBoard
  }
}
