package main.extensions

/**
 * Created by kdoherty on 10/25/15.
 */
object ImplicitIterator {

  implicit def extendIterator[A](i : Iterator[A]): IteratorExtension[A] = new IteratorExtension(i)

}
