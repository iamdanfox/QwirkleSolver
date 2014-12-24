package quirkle

trait Shape
object Square extends Shape {
  override def toString: String = "1"
}
object Diamond extends Shape {
  override def toString: String = "2"
}
object Circle extends Shape {
  override def toString: String = "3"
}
object Clubs extends Shape {
  override def toString: String = "4"
}
object Spiky extends Shape {
  override def toString: String = "5"
}
object Cross extends Shape {
  override def toString: String = "6"
}
