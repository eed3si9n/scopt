package scopt

trait Zero[A] {
  def zero: A
}
object Zero {
  def zero[A](f: => A): Zero[A] = new Zero[A] {
    val zero = f
  }
  implicit val intZero: Zero[Int]             = zero(0)
  implicit val unitZero: Zero[Unit]           = zero(())
}
