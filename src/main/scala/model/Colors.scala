package main.model

/**
 * Created by kdoherty on 10/23/15.
 */
object Colors extends Enumeration {

  val WHITE, BLACK = Value

  def opp(color: Value) = {
    if (color == WHITE) BLACK else WHITE
  }

}
