package qwirkle

trait Colour
object Red extends Colour {
  override def toString: String = "R"
  override def hashCode: Int = 10
}
object Green extends Colour {
  override def toString: String = "G"
  override def hashCode: Int = 20
}
object Blue extends Colour {
  override def toString: String = "B"
  override def hashCode: Int = 30
}
object Orange extends Colour {
  override def toString: String = "O"
  override def hashCode: Int = 40
}
object Yellow extends Colour {
  override def toString: String = "Y"
  override def hashCode: Int = 50
}
object Purple extends Colour {
  override def toString: String = "P"
  override def hashCode: Int = 60
}
