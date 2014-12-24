package qwirkle


case class Piece(colour: Colour, shape: Shape) {
  override def toString: String = colour.toString() + shape.toString()
}

trait Move {}
case class PlacePieces(startSquare: (Int, Int), direction: Direction, pieces: List[Piece]) extends Move
object SwapPieces extends Move