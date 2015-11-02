package main.extensions

/**
 * Created by kdoherty on 10/25/15.
 */
class IteratorExtension[A](i : Iterator[A]) {
  def takeWhileInclusive(p: A => Boolean) = {
    val (a, b) = i.span(p)
    a ++ (b take 1)
  }
}
