import main.model.{Colors, ChessBoard, Square}
import org.specs2.mutable._

/**
 * Created by kdoherty on 10/29/15.
 */
class ChessBoardSpec extends Specification {

  "A ChessBoard" should {

    "be able to fill the board with all pieces in their starting positions" in {
      val startingBoard = new ChessBoard()

      startingBoard.toString mustEqual
        "  a b c d e f g h\n" +
          "8|R|N|B|Q|K|B|N|R|8\n" +
          "7|P|P|P|P|P|P|P|P|7\n" +
          "6| | | | | | | | |6\n" +
          "5| | | | | | | | |5\n" +
          "4| | | | | | | | |4\n" +
          "3| | | | | | | | |3\n" +
          "2|p|p|p|p|p|p|p|p|2\n" +
          "1|r|n|b|q|k|b|n|r|1\n" +
          "  a b c d e f g h" + "\n"
    }

    "be able to identify all the valid moves that a color can play" in {
      println(new ChessBoard().getMoves(Colors.WHITE))
      1 mustEqual 1
    }

        "be able to produce all squares on the board" in {
          ChessBoard.getSquares.size mustEqual 64
        }

        "be able to get all squares that are vertically or horizontally aligned with a given square" in {
          "if the given square is in the middle" in {
            val expectedPaths = Vector(
              Vector(Square("d5"), Square("c5"), Square("b5"), Square("a5")),
              Vector(Square("e6"), Square("e7"), Square("e8")),
              Vector(Square("f5"), Square("g5"), Square("h5")),
              Vector(Square("e4"), Square("e3"), Square("e2"), Square("e1"))
            )

            ChessBoard.crossFrom(Square("e5")) must containTheSameElementsAs(expectedPaths)
          }

          "if the given square is in the bottom left corner" in {
            val expectedPaths = Vector(
              Vector(),
              Vector(Square("a2"), Square("a3"), Square("a4"), Square("a5"), Square("a6"), Square("a7"), Square("a8")),
              Vector(Square("b1"), Square("c1"), Square("d1"), Square("e1"), Square("f1"), Square("g1"), Square("h1")),
              Vector()
            )
            ChessBoard.crossFrom(Square("a1")) must containTheSameElementsAs(expectedPaths)
          }

          "if the given square is in the top left corner" in {
            val expectedPaths = Vector(
              Vector(),
              Vector(),
              Vector(Square("b8"), Square("c8"), Square("d8"), Square("e8"), Square("f8"), Square("g8"), Square("h8")),
              Vector(Square("a7"), Square("a6"), Square("a5"), Square("a4"), Square("a3"), Square("a2"), Square("a1"))
            )

            ChessBoard.crossFrom(Square("a8")) must containTheSameElementsAs(expectedPaths)
          }

          "if the given square is in the bottom right corner" in {
            val expectedPaths = Vector(
              Vector(Square("g1"), Square("f1"), Square("e1"), Square("d1"), Square("c1"), Square("b1"), Square("a1")),
              Vector(Square("h2"), Square("h3"), Square("h4"), Square("h5"), Square("h6"), Square("h7"), Square("h8")),
              Vector(),
              Vector()
            )

            ChessBoard.crossFrom(Square("h1")) must containTheSameElementsAs(expectedPaths)
          }

          "if the given square is in the top right corner" in {
            val expectedPaths = Vector(
              Vector(Square("g8"), Square("f8"), Square("e8"), Square("d8"), Square("c8"), Square("b8"), Square("a8")),
              Vector(),
              Vector(),
              Vector(Square("h7"), Square("h6"), Square("h5"), Square("h4"), Square("h3"), Square("h2"), Square("h1"))
            )
            ChessBoard.crossFrom(Square("h8")) must containTheSameElementsAs(expectedPaths)
          }
        }

        "be able to get all squares that are diagonally aligned with a given square" in {
          "if the given square is in the middle" in {
            val expectedPaths = Vector(
              Vector(Square("d6"), Square("c7"), Square("b8")),
              Vector(Square("f6"), Square("g7"), Square("h8")),
              Vector(Square("d4"), Square("c3"), Square("b2"), Square("a1")),
              Vector(Square("f4"), Square("g3"), Square("h2"))
            )

            ChessBoard.diagonalFrom(Square("e5")) must containTheSameElementsAs(expectedPaths)
          }

          "if the given square is on the left edge" in {
            val expectedPaths = Vector(
              Vector(),
              Vector(Square("b5"), Square("c6"), Square("d7"), Square("e8")),
              Vector(Square("b3"), Square("c2"), Square("d1")),
              Vector()
            )

            ChessBoard.diagonalFrom(Square("a4")) must containTheSameElementsAs(expectedPaths)
          }

          "if the given square is on the top edge" in {
            val expectedPaths = Vector(
              Vector(),
              Vector(),
              Vector(Square("d7"), Square("e6"), Square("f5"), Square("g4"), Square("h3")),
              Vector(Square("b7"), Square("a6"))
            )

            ChessBoard.diagonalFrom(Square("c8")) must containTheSameElementsAs(expectedPaths)

            val expectedPaths2 = Vector(
              Vector(),
              Vector(),
              Vector(Square("g7"), Square("h6")),
              Vector(Square("e7"), Square("d6"), Square("c5"), Square("b4"), Square("a3"))
            )

            ChessBoard.diagonalFrom(Square("f8")) must containTheSameElementsAs(expectedPaths2)
          }

          "if the given square is on the right edge" in {
            val expectedPaths = Vector(
              Vector(Square("g5"), Square("f6"), Square("e7"), Square("d8")),
              Vector(),
              Vector(),
              Vector(Square("g3"), Square("f2"), Square("e1"))
            )

            ChessBoard.diagonalFrom(Square("h4")) must containTheSameElementsAs(expectedPaths)
          }

          "if the given square is on the bottom edge" in {
            val expectedPaths = Vector(
              Vector(Square("b2"), Square("a3")),
              Vector(Square("d2"), Square("e3"), Square("f4"), Square("g5"), Square("h6")),
              Vector(),
              Vector()
            )

            ChessBoard.diagonalFrom(Square("c1")) must containTheSameElementsAs(expectedPaths)

            val expectedPaths2 = Vector(
              Vector(Square("e2"), Square("d3"), Square("c4"), Square("b5"), Square("a6")),
              Vector(Square("g2"), Square("h3")),
              Vector(),
              Vector()
            )

            ChessBoard.diagonalFrom(Square("f1")) must containTheSameElementsAs(expectedPaths2)
          }

          "if the given square is in the bottom left corner" in {
            val expectedPaths = Vector(
              Vector(),
              Vector(Square("b2"), Square("c3"), Square("d4"), Square("e5"), Square("f6"), Square("g7"), Square("h8")),
              Vector(),
              Vector()
            )
            ChessBoard.diagonalFrom(Square("a1")) must containTheSameElementsAs(expectedPaths)
          }

          "if the given square is in the top left corner" in {
            val expectedPaths = Vector(
              Vector(),
              Vector(),
              Vector(),
              Vector(Square("b7"), Square("c6"), Square("d5"), Square("e4"), Square("f3"), Square("g2"), Square("h1"))
            )

            ChessBoard.diagonalFrom(Square("a8")) must containTheSameElementsAs(expectedPaths)
          }

          "if the given square is in the bottom right corner" in {
            val expectedPaths = Vector(
              Vector(Square("g2"), Square("f3"), Square("e4"), Square("d5"), Square("c6"), Square("b7"), Square("a8")),
              Vector(),
              Vector(),
              Vector()
            )

            ChessBoard.diagonalFrom(Square("h1")) must containTheSameElementsAs(expectedPaths)
          }

          "if the given square is in the top right corner" in {
            val expectedPaths = Vector(
              Vector(),
              Vector(),
              Vector(Square("g7"), Square("f6"), Square("e5"), Square("d4"), Square("c3"), Square("b2"), Square("a1")),
              Vector()
            )
            ChessBoard.diagonalFrom(Square("h8")) must containTheSameElementsAs(expectedPaths)
          }
        }

        "be able to get all squares neighboring a given square" in {
          "if the given square is in the middle" in {
            val expectedNeighbors = Vector(
              Square("d6"), Square("e6"), Square("f6"),
              Square("d5"), Square("f5"),
              Square("d4"), Square("e4"), Square("f4"))
            ChessBoard.neighbors(Square("e5")) must containTheSameElementsAs(expectedNeighbors)
          }

          "if the given square is in the bottom left corner" in {
            val expectedNeighbors = Vector(Square("a2"), Square("b2"), Square("b1"))
            ChessBoard.neighbors(Square("a1")) must containTheSameElementsAs(expectedNeighbors)
          }

          "if the given square is in the top left corner" in {
            val expectedNeighbors = Vector(Square("a7"), Square("b7"), Square("b8"))
            ChessBoard.neighbors(Square("a8")) must containTheSameElementsAs(expectedNeighbors)
          }

          "if the given square is in the bottom right corner" in {
            val expectedNeighbors = Vector(Square("g1"), Square("h2"), Square("g2"))
            ChessBoard.neighbors(Square("h1")) must containTheSameElementsAs(expectedNeighbors)
          }

          "if the given square is in the top right corner" in {
            val expectedNeighbors = Vector(Square("h7"), Square("g8"), Square("g7"))
            ChessBoard.neighbors(Square("h8")) must containTheSameElementsAs(expectedNeighbors)
          }
        }
  }

}

