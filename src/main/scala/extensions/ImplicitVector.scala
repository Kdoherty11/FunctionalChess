package main.extensions

/**
 * Created by kdoherty on 10/25/15.
 */
object ImplicitVector {

  implicit def extendIterator[A](v : Vector[A]): VectorExtension[A] = new VectorExtension[A](v)

}
