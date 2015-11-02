package main.model.pieces

import main.model._

/**
 * Created by kdoherty on 10/23/15.
 */
case class Knight(override val color : Colors.Value) extends ChessPiece {

  override val symbol: Char = 'n'

  override def getAttackedSquares(board: ChessBoard, sq: Square): Vector[Square] = {
    val r = sq.row
    val c = sq.col

    val combinations = for {
      l <- Vector(2, -2)
      s <- Vector(1, -1)
    } yield (l, s)

    val allCombinations = combinations ++ combinations.map(tup => (tup._2, tup._1))

    def inboundPt(pt: Tuple2[Int, Int]): Boolean = {
      ChessBoard.isRowInbounds(r + pt._1) && ChessBoard.isColInbounds(c + pt._2)
    }

    def inboundSqs: PartialFunction[Tuple2[Int, Int], Square] = {
      case pt if inboundPt(pt) => new Square(r + pt._1, c + pt._2)
    }

    allCombinations
      .collect(inboundSqs)
      .filterNot(board.containsPiece(_, color))
  }

  override def makeMove(move: Move): ChessBoard = move.board.movePieceUpdatedBoard(move.from, move.to)

}