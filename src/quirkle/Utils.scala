package quirkle


case class Piece(colour: Colour, shape: Shape) {
  override def toString: String = colour.toString() + shape.toString()
}

trait Move {}
case class PlacePieces(startSquare: (Int, Int), direction: Direction, pieces: List[Piece]) extends Move
object SwapPieces extends Move



object Utils {

  val NUM_PIECES_PER_PLAYER = 6

  def resupplyPlayerBag(bag: List[Piece], playerBag: List[Piece]): (List[Piece], List[Piece]) = {
    val takenOutOfBag = scala.util.Random.shuffle(bag).take(NUM_PIECES_PER_PLAYER - playerBag.length)
    return (bag.diff(takenOutOfBag), playerBag ++ takenOutOfBag)
  }

  def makeInitialGameState(numPlayers: Int): GameState = {
    val distinctPieces = for (
      color <- List(Red, Orange, Yellow, Green, Blue, Purple);
      shape <- List(Square, Circle, Diamond, Clubs, Spiky, Cross)
    ) yield Piece(color, shape)

    val startBag = scala.util.Random.shuffle(distinctPieces ++ distinctPieces ++ distinctPieces)

    val (playerBags, leftOvers) = startBag.grouped(NUM_PIECES_PER_PLAYER).toList.splitAt(numPlayers)
    val bag = leftOvers.flatten

    val board = new Board(new scala.collection.immutable.HashMap[(Int, Int), Piece])

    return new GameState(board, playerBags.toArray, bag, 0)
  }
}