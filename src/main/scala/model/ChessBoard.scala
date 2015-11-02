package main.model

import main.model.pieces._
import main.model.ChessBoard._

import scala.collection.immutable.Queue

/**
 * Created by kdoherty on 10/23/15.
 */
case class ChessBoard(pieces: Vector[Vector[Option[ChessPiece]]] = ChessBoard.startingPieces,
                      sideToMove: Colors.Value = Colors.WHITE,
                      whiteKingSq: Square = ChessBoard.WhiteKingStartSquare,
                      blackKingSq: Square = ChessBoard.BlackKingStartSquare,
                      whiteCastled: Boolean = false,
                      blackCastled: Boolean = false,
                      enPassantSqOpt: Option[Square] = Option.empty,
                      playedMoves: Queue[Move] = Queue.empty[Move]) {

  def getPiece(sq: Square): Option[ChessPiece] = pieces(sq.row)(sq.col)

  def isEmpty(sq: Square) = getPiece(sq).isEmpty

  def containsPiece(sq: Square, color: Colors.Value) = {
    val piece = getPiece(sq)
    piece.isDefined && piece.get.color == color
  }

  def setPiece(sq: Square, pieceOpt: Option[ChessPiece]): ChessBoard =
    copy(pieces = pieces.updated(sq.row, pieces(sq.row).updated(sq.col, pieceOpt)))

  def remove(sq: Square): ChessBoard = setPiece(sq, None)

  // Moves a main.model.pieces.Piece from one square to another
  def movePiece(from: Square, to: Square) = setPiece(to, getPiece(from)).remove(from)

  // Moves a piece from square to another, sets the new side to move, and enqueue's the moves to the move list
  def movePieceUpdatedBoard(from: Square, to: Square, moveFromTo: (Square, Square) => ChessBoard = movePiece): ChessBoard = {
    moveFromTo(from, to).copy(
      sideToMove = Colors.opp(sideToMove),
      playedMoves = playedMoves.enqueue(new Move(this, from, to)))
  }

  // Returns Vector of (Piece, Square)
  def getPieces(color: Colors.Value): Vector[(ChessPiece, Square)] = {
    getSquares
      .withFilter(containsPiece(_, color))
      .map(sq => (getPiece(sq).get, sq))
  }

  def getMoves(color: Colors.Value): Vector[Move] = {
    getPieces(color).flatMap { case (p, from) => p.getMoves(this, from) }
  }

  def getAttackedSquares(color: Colors.Value): Vector[Square] = {
    getPieces(color).flatMap { case (p, from) => p.getAttackedSquares(this, from) }
  }

  def kingInCheck(color: Colors.Value): Boolean = isAttackedBy(getKingSq(color), Colors.opp(color))

  def isAttackedBy(sq: Square, color: Colors.Value): Boolean = getAttackedSquares(color).contains(sq)

  def getKingSq(color: Colors.Value): Square = if (color == Colors.WHITE) whiteKingSq else blackKingSq

  def isOver: Boolean = getMoves(sideToMove).isEmpty

  override def toString = {
    val sb = new StringBuilder

    val colLabels = "  a b c d e f g h\n"

    sb.append(colLabels)

    RowRange.foreach(r => {
      sb.append(8 - r)
      sb.append("|")
      ColRange.foreach(c => {
        val cur = new Square(r, c)
        val pieceOpt = getPiece(cur)
        if (pieceOpt.isDefined) {
          sb.append(pieceOpt.get)
        } else {
          sb.append(" ")
        }
        sb.append("|")

      })

      sb.append(8 - r)
      sb.append("\n")
    })
    sb.append(colLabels)

    sb.toString()
  }

}

object ChessBoard {

  val NumRows = 8
  val NumCols = 8

  val MaxRowIndex = NumRows - 1
  val MaxColIndex = NumCols - 1

  val RowRange = 0 to NumRows - 1
  val ColRange = 0 to NumCols - 1

  val ColNames = "abcdefgh"

  val WhiteKingStartSquare = Square("e1")
  val BlackKingStartSquare = Square("e8")

  def isRowInbounds(row: Int) = {
    row >= 0 && row < NumCols
  }

  def isColInbounds(col: Int) = {
    col >= 0 && col < NumCols
  }

  def isInbounds(sq: Square) = {
    isRowInbounds(sq.row) && isColInbounds(sq.col)
  }

  def homeRow(color: Colors.Value): Int = if (color == Colors.WHITE) MaxRowIndex else 0

  def pawnHomeRow(color: Colors.Value): Int = if (color == Colors.WHITE) MaxRowIndex - 1 else 1

  def finalRow(color: Colors.Value): Int = homeRow(Colors.opp(color))

  def left(sq: Square, color: Colors.Value): Option[Square] = {
    val offset = if (color == Colors.WHITE) -1 else 1

    if (isColInbounds(sq.col + offset)) Some(new Square(sq.row, sq.col + offset)) else None
  }

  def right(sq: Square, color: Colors.Value): Option[Square] = {
    val offset = if (color == Colors.WHITE) 1 else -1

    if (isColInbounds(sq.col + offset)) Some(new Square(sq.row, sq.col + offset)) else None
  }

  def ahead(sq: Square, color: Colors.Value): Option[Square] = {
    val offset = if (color == Colors.WHITE) -1 else 1

    if (isRowInbounds(sq.row + offset)) Some(new Square(sq.row + offset, sq.col)) else None
  }

  def behind(sq: Square, color: Colors.Value): Option[Square] = {
    val offset = if (color == Colors.WHITE) 1 else -1

    if (isRowInbounds(sq.row + offset)) Some(new Square(sq.row + offset, sq.col)) else None
  }

  def getSquares: Vector[Square] = {
    (for {
      row <- RowRange
      col <- ColRange
    } yield {
        new Square(row, col)
      }).toVector
  }

  def crossFrom(sq: Square): Vector[Vector[Square]] = {
    val top = new Square(0, sq.col)
    val bottom = new Square(MaxRowIndex, sq.col)

    val left = new Square(sq.row, 0)
    val right = new Square(sq.row, MaxColIndex)

    Vector(
      sq.horizontalPath(left),
      sq.verticalPath(top),
      sq.horizontalPath(right),
      sq.verticalPath(bottom))
      .collect { case Some(path) => path }
  }

  def diagonalFrom(sq: Square): Vector[Vector[Square]] = {
    val topLeftOffset = math.min(sq.row, sq.col)
    val bottomRightOffset = math.min(MaxRowIndex - sq.row, MaxColIndex - sq.col)
    val isTopLeftHalf = sq.row + sq.col <= MaxRowIndex
    val rowColSum = sq.row + sq.col

    val topLeft = new Square(sq.row - topLeftOffset, sq.col - topLeftOffset)
    val bottomRight = new Square(sq.row + bottomRightOffset, sq.col + bottomRightOffset)
    val topRight = if (isTopLeftHalf) new Square(0, rowColSum) else new Square(rowColSum - 7, 7)
    val bottomLeft = if (isTopLeftHalf) new Square(rowColSum, 0) else new Square(7, rowColSum - 7)

    Vector(sq.diagonalPath(topLeft),
      sq.diagonalPath(topRight),
      sq.diagonalPath(bottomRight),
      sq.diagonalPath(bottomLeft))
      .map(_.get)
  }

  def neighbors(sq: Square): Vector[Square] = {
    val startRow = if (sq.row > 0) sq.row - 1 else 0
    val endRow = if (sq.row < MaxRowIndex) sq.row + 1 else MaxRowIndex

    val startCol = if (sq.col > 0) sq.col - 1 else 0
    val endCol = if (sq.col < MaxColIndex) sq.col + 1 else MaxColIndex

    (for {
      r <- startRow to endRow
      c <- startCol to endCol
      if !(r == sq.row && c == sq.col)
    } yield {
        new Square(r, c)
      }).toVector
  }

  def mapToPieces(pieceMap: Map[Square, ChessPiece]): Vector[Vector[Option[ChessPiece]]] = {
      RowRange.map(r =>
        ColRange.map(c => {
          pieceMap.get(new Square(r, c))
        }).toVector
      ).toVector
  }

  def withPieces(pieceMap: Map[Square, ChessPiece]): ChessBoard = {
    new ChessBoard(pieces = ChessBoard.mapToPieces(pieceMap))
  }

  def startingPieces = {

    val whitePawns = ColRange.map(col => new Square(6, col) -> new Pawn(Colors.WHITE))
    val blackPawns = ColRange.map(col => new Square(1, col) -> new Pawn(Colors.BLACK))
    val bishops = Map(
      Square("c1") -> Bishop(Colors.WHITE),
      Square("f1") -> Bishop(Colors.WHITE),
      Square("c8") -> Bishop(Colors.BLACK),
      Square("f8") -> Bishop(Colors.BLACK)
    )

    val knights = Map(
      Square("b1") -> Knight(Colors.WHITE),
      Square("g1") -> Knight(Colors.WHITE),
      Square("b8") -> Knight(Colors.BLACK),
      Square("g8") -> Knight(Colors.BLACK)
    )

    val rooks = Map(
      Square("a1") -> Rook(Colors.WHITE),
      Square("h1") -> Rook(Colors.WHITE),
      Square("a8") -> Rook(Colors.BLACK),
      Square("h8") -> Rook(Colors.BLACK)
    )

    val queens = Map(
      Square("d1") -> Queen(Colors.WHITE),
      Square("d8") -> Queen(Colors.BLACK)
    )

    val kings = Map(
      WhiteKingStartSquare -> King(Colors.WHITE),
      BlackKingStartSquare -> King(Colors.BLACK)
    )

    val pieceMap = (whitePawns ++ blackPawns ++ bishops ++ knights ++ rooks ++ queens ++ kings).toMap

    RowRange.map(r =>
      ColRange.map(c => {
        pieceMap.get(new Square(r, c))
      }).toVector
    ).toVector
  }
}
