import main.model.pieces.{Pawn, Bishop}
import main.model.{Colors, ChessBoard, Square}
import org.specs2.mutable._


/**
 * Created by kdoherty on 10/31/15.
 */
class BishopSpec extends Specification {

  "A Bishop" should {

    "only attack unblocked diagonal squares" in {

      val sq = Square("e4")
      val bishop = Bishop(Colors.WHITE)

      val pieceMap = Map(
        sq -> bishop,
        Square("g6") -> Pawn(Colors.WHITE),
        Square("g2") -> Pawn(Colors.BLACK),
        Square("d3") -> Pawn(Colors.WHITE)
      )

      val board = ChessBoard.withPieces(pieceMap)

      val expectedSqs = Vector(Square("d5"), Square("c6"), Square("b7"), Square("a8"), Square("f5"), Square("f3"), Square("g2"))
      bishop.getAttackedSquares(board, sq) must containTheSameElementsAs(expectedSqs)
    }
  }


}
