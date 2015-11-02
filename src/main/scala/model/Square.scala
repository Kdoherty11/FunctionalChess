package main.model

/**
 * Created by kdoherty on 10/23/15.
 */
case class Square(row: Int, col: Int) {

  def this(tup: Tuple2[Int, Int]) = this(tup._1, tup._2)

  private def this(colChar: Char, rowNum: Int) = {
    this(ChessBoard.NumRows - rowNum, ChessBoard.ColNames.indexOf(Character.toLowerCase(colChar)))
  }

  def this(strRep: String) = {
    this(strRep.charAt(0): Char, strRep.charAt(1).asDigit)
  }

  if (!ChessBoard.isInbounds(this)) {
    throw new IllegalArgumentException("Square Out of Bounds. Row: " + row + " Col: " + col)
  }

  def horizontalDist(to: Square): Int = math.abs(col - to.col)

  def verticalDist(to: Square): Int = math.abs(row - to.row)

  def isHorizontal(other: Square): Boolean = row == other.row

  def isVertical(other: Square): Boolean = col == other.col

  def isDiagonal(other: Square): Boolean = verticalDist(other) == horizontalDist(other)

  def horizontalPath(to: Square): Option[Vector[Square]] = {
    if (!isHorizontal(to)) {
      return None
    }

    if (to == this) {
      return Some(Vector.empty)
    }

    val colIncrement = if (col < to.col) 1 else -1
    val colRange = col + colIncrement to to.col by colIncrement

    Some(colRange.map(new Square(row, _)).toVector)
  }

  def verticalPath(to: Square): Option[Vector[Square]] = {
    if (!isVertical(to)) {
      return None
    }

    if (to == this) {
      return Some(Vector.empty)
    }

    val rowIncrement = if (row < to.row) 1 else -1
    val rowRange = row + rowIncrement to to.row by rowIncrement

    Some(rowRange.map(new Square(_, col)).toVector)
  }

  def diagonalPath(to: Square): Option[Vector[Square]] = {
    if (!isDiagonal(to)) {
      return None
    }

    if (to == this) {
      return Some(Vector.empty)
    }

    val rowIncrement = if (row < to.row) 1 else -1
    val colIncrement = if (col < to.col) 1 else -1

    val rowRange = row + rowIncrement to to.row by rowIncrement
    val colRange = col + colIncrement to to.col by colIncrement

    Some(rowRange.zip(colRange).map(new Square(_)).toVector)
  }

  override def hashCode: Int = 41 * (41 + row) + col

  override def equals(other: Any): Boolean = other match {
    case that: Square =>
      (this.row == that.row) && (this.col == that.col)
    case _ =>
      false
  }

  override def toString: String = {
    val colName = ChessBoard.ColNames.charAt(col).toString
    val rowName = ChessBoard.NumRows - row
    colName + rowName
  }

}

object Square {
  def apply(strRep: String): Square = new Square(strRep)
}
