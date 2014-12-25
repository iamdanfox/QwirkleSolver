package qwirkle


case class Piece(colour: Colour, shape: Shape) {
  override def toString: String = colour.toString() + shape.toString()
  override def hashCode: Int = colour.hashCode + shape.hashCode
}

trait Move {}
// maybe store these differently (line of color / line of shape???)
case class PlacePieces(startSquare: (Int, Int), direction: Direction, pieces: List[Piece]) extends Move
object SwapPieces extends Move
