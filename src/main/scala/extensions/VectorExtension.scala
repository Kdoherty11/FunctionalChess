package main.extensions

/**
 * Created by kdoherty on 10/25/15.
 */
class VectorExtension[A](v : Vector[A]) {

  def takeWhileInclusive(p: A => Boolean) = {
    val (a, b) = v.span(p)
    a ++ (b take 1)
  }

}
