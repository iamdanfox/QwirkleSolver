package quirkle

trait Direction {
  def apply(square: (Int, Int)): (Int, Int)
  
  def applyStream(square: (Int, Int)): Stream[(Int,Int)] = {
    square #:: applyStream(apply(square))
  } 
}
object Up extends Direction {
  def apply(square: (Int, Int)): (Int, Int) = (square._1, square._2 + 1)
  override def toString: String = "^"
}
object Down extends Direction {
  def apply(square: (Int, Int)): (Int, Int) = (square._1, square._2 - 1)
  override def toString: String = "v"
}
object Left extends Direction {
  def apply(square: (Int, Int)): (Int, Int) = (square._1 - 1, square._2)
  override def toString: String = "<"
}
object Right extends Direction {
  def apply(square: (Int, Int)): (Int, Int) = (square._1 + 1, square._2)
  override def toString: String = ">"
}