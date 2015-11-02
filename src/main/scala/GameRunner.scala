import main.model.pieces._
import main.model.{Colors, Move, Square, ChessBoard}

/**
 * Created by kdoherty on 10/31/15.
 */
object GameRunner {

  var debug = false

  def makeUserMove(board: ChessBoard): Unit = {
    if (board.isOver) {
      println("Game Over!")
      if (board.kingInCheck(board.sideToMove)) {
        println(Colors.opp(board.sideToMove).toString.toLowerCase.capitalize + " Wins")
      } else {
        println("Draw...")
      }
      println(board.toString())
    } else {
      val move = getLegalMoveFromUser(board)
      makeUserMove(move.made)
    }
  }

  def getLegalMoveFromUser(board: ChessBoard): Move = {
    println(board.toString())
    println("What is your move: ")
    val moveStr = scala.io.StdIn.readLine()
    if (moveStr == "moves") {
      println(board.getMoves(board.sideToMove))
      return getLegalMoveFromUser(board)
    }
    if (moveStr == "debug") {
      debug = !debug
      return getLegalMoveFromUser(board)
    }
    try {
      val fromSq = Square(moveStr.substring(0, 2))
      val toSq = Square(moveStr.substring(2))

      val move = new Move(board, fromSq, toSq)
      if (!move.isLegal && !debug) {
        println(move + " is not a legal move")
        return getLegalMoveFromUser(board)
      }
      if (move.piece.color != board.sideToMove && !debug) {
        println(move + " is not legal because it is " + board.sideToMove + "'s move")
        return getLegalMoveFromUser(board)
      }
      if (move.isPromotion) {
        val promotionPiece = getPromotionPieceChoice(move.piece.color)
        new Move(board, fromSq, toSq, Some(promotionPiece))
      } else {
        move
      }
    } catch {
      case _: Throwable =>
        println("Invalid Move. Please try again")
        getLegalMoveFromUser(board)
    }
  }

  def getPromotionPieceChoice(color: Colors.Value): ChessPiece = {
    val promotionPieceSym = scala.io.StdIn.readChar()
    val promotionPieceOpt = parsePieceSymbol(promotionPieceSym, color)
    if (promotionPieceOpt.isDefined) {
      promotionPieceOpt.get
    } else {
      println(promotionPieceSym + " is not a valid promotion piece. Please enter one of [q, r, b, n]")
      getPromotionPieceChoice(color)
    }
  }

  def parsePieceSymbol(sym: Char, color: Colors.Value): Option[ChessPiece] = {
    sym.toLower match {
      case 'q' => Some(new Queen(color))
      case 'r' => Some(new Rook(color))
      case 'b' => Some(new Bishop(color))
      case 'n' => Some(new Knight(color))
      case _ => None
    }
  }

  def main(args: Array[String]): Unit = {
    println("Game Starting")
    makeUserMove(new ChessBoard())
  }





}
