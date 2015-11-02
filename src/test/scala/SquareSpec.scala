import _root_.main.model.Square
import org.specs2.mutable._

/**
 * Created by kdoherty on 10/26/15.
 */
class SquareSpec extends Specification {

  "A Square" should {

    "be able to be created with a tuple" in {
      val s = new Square((1, 2))
      s.row mustEqual 1
      s.col mustEqual 2
    }

    "return the vertical distance to another square" in {
      val s1 = new Square(2, 3)
      val s2 = new Square(7, 3)
      s1.verticalDist(s2) mustEqual 5
      s2.verticalDist(s1) mustEqual 5
    }

    "return the horizontal distance to another square" in {
      val s1 = new Square(2, 3)
      val s2 = new Square(2, 5)
      s1.horizontalDist(s2) mustEqual 2
      s2.horizontalDist(s1) mustEqual 2
    }

    "determine if it is vertically aligned with another square" in {
      val s1 = new Square(2, 3)
      val s2 = new Square(7, 3)
      val s3 = new Square(5, 5)
      s1.isVertical(s1) mustEqual true
      s1.isVertical(s2) mustEqual true
      s2.isVertical(s1) mustEqual true
      s1.isVertical(s3) mustEqual false
    }

    "determine if it is horizontally aligned with another square" in {
      val s1 = new Square("a1")
      val s2 = new Square("h1")
      val s3 = new Square("a2")
      s1.isHorizontal(s1) mustEqual true
      s1.isHorizontal(s2) mustEqual true
      s2.isHorizontal(s1) mustEqual true
      s1.isHorizontal(s3) mustEqual false
      s3.isHorizontal(s1) mustEqual false
    }

    "determine if it is on the same diagonal as another square" in {
      val s1 = new Square(3, 0)
      val s2 = new Square(7, 4)
      val s3 = new Square(5, 3)
      s1.isDiagonal(s1) mustEqual true
      s1.isDiagonal(s2) mustEqual true
      s2.isDiagonal(s1) mustEqual true
      s1.isDiagonal(s3) mustEqual false
    }

    "be able to produce the squares diagonally between itself and another square starting from itself" in {
      val start = new Square(3, 3)
      val upperLeft = new Square(0, 0)
      val upperRight = new Square(0, 6)
      val lowerLeft = new Square(6, 0)
      val lowerRight = new Square(7, 7)

      val upperLeftExpected = Vector(new Square(2, 2), new Square(1, 1), upperLeft)
      val upperRightExpected = Vector(new Square(2, 4), new Square(1, 5), upperRight)
      val lowerLeftExpected = Vector(new Square(4, 2), new Square(5, 1), lowerLeft)
      val lowerRightExpected = Vector(new Square(4, 4), new Square(5, 5), new Square(6, 6), lowerRight)

      start.diagonalPath(upperLeft).get must containAllOf(upperLeftExpected).inOrder
      start.diagonalPath(upperRight).get must containAllOf(upperRightExpected).inOrder
      start.diagonalPath(lowerLeft).get must containAllOf(lowerLeftExpected).inOrder
      start.diagonalPath(lowerRight).get must containAllOf(lowerRightExpected).inOrder
    }


    "be able to produce the squares horizontally between itself and another square starting from itself" in {
      val leftEdge = new Square("a1")
      val rightEdge = new Square("h1")

      val leftRightExpected = Vector(Square("b1"), Square("c1"), Square("d1"), Square("e1"), Square("f1"), Square("g1"), Square("h1"))
      val rightLeftExpected = Vector(Square("g1"), Square("f1"), Square("e1"), Square("d1"), Square("c1"), Square("b1"), Square("a1"))

      leftEdge.horizontalPath(rightEdge).get must containAllOf(leftRightExpected).inOrder
      rightEdge.horizontalPath(leftEdge).get must containAllOf(rightLeftExpected).inOrder
    }

    "be able to produce the squares vertically between itself and another square starting from itself" in {
      val bottomEdge = Square("e1")
      val topEdge = Square("e8")

      val upExpected = Vector(Square("e2"), Square("e3"), Square("e4"), Square("e5"), Square("e6"), Square("e7"), Square("e8"))
      val downExpected = Vector(Square("e7"), Square("e6"), Square("e5"), Square("e4"), Square("e3"), Square("e2"))

      bottomEdge.verticalPath(topEdge).get must containAllOf(upExpected).inOrder
      topEdge.verticalPath(bottomEdge).get must containAllOf(downExpected).inOrder
    }

  }
}
