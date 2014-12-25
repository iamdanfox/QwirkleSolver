package qwirkle

trait Shape
object Square extends Shape {
  override def toString: String = "1"
  override def hashCode: Int = 1
}
object Diamond extends Shape {
  override def toString: String = "2"
  override def hashCode: Int = 2
}
object Circle extends Shape {
  override def toString: String = "3"
  override def hashCode: Int = 3
}
object Clubs extends Shape {
  override def toString: String = "4"
  override def hashCode: Int = 4
}
object Spiky extends Shape {
  override def toString: String = "5"
  override def hashCode: Int = 5
}
object Cross extends Shape {
  override def toString: String = "6"
  override def hashCode: Int = 6
}
