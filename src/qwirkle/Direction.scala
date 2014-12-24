package qwirkle

trait Direction {
  def apply(square: (Int, Int)): (Int, Int)

  def applyStream(square: (Int, Int)): Stream[(Int, Int)] = {
    square #:: applyStream(apply(square))
  }

  def opposite: Direction
}
object Up extends Direction {
  def apply(square: (Int, Int)): (Int, Int) = (square._1, square._2 + 1)
  override def toString: String = "^"
  def opposite: Direction = Down
}
object Down extends Direction {
  def apply(square: (Int, Int)): (Int, Int) = (square._1, square._2 - 1)
  override def toString: String = "v"
  def opposite: Direction = Up
}
object Left extends Direction {
  def apply(square: (Int, Int)): (Int, Int) = (square._1 - 1, square._2)
  override def toString: String = "<"
  def opposite: Direction = Right
}
object Right extends Direction {
  def apply(square: (Int, Int)): (Int, Int) = (square._1 + 1, square._2)
  override def toString: String = ">"
  def opposite: Direction = Left
}