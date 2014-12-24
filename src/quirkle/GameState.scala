package quirkle

case class Piece(colour: Colour, shape: Shape) {
  override def toString: String = colour.toString() + shape.toString()
}

object Utils {

  def resupplyPlayerBag(bag: List[Piece], playerBag: List[Piece], playerNumber: Int): (List[Piece], List[Piece]) = {
    val takenOutOfBag = scala.util.Random.shuffle(bag).take(playerNumber - playerBag.length)
    return (bag.diff(takenOutOfBag), playerBag ++ takenOutOfBag)
  }
}

trait Move {}
case class PlacePieces(startSquare: (Int, Int), direction: Direction, pieces: List[Piece]) extends Move
object SwapPieces extends Move

case class Board(map: scala.collection.Map[(Int, Int), Piece]) {

  def put(square: (Int, Int), direction: Direction, listOfPieces: List[Piece]): Board = {
    val newMap = this.map ++ direction.applyStream(square).zip(listOfPieces)
    return new Board(newMap)
  }

  def allowsMove(move: Move): Boolean = move match {
    case PlacePieces(square, direction, pieces) => true
    case SwapPieces => true
  }

  def getPerimeter(): List[(Int, Int)] = {
    var perimeter = List[(Int, Int)]()

    def getNeighbours(square: (Int, Int)): List[(Int, Int)] =
      List(Up, Down, Left, Right).map(_.apply(square))

    map.foreach {
      case (square, _) =>
        perimeter = perimeter ++ getNeighbours(square).filter { !map.contains(_) }
    }
    return perimeter
  }

  def getStartConfigurations(): List[((Int, Int), Direction)] = {
    val perimeter = getPerimeter()
    val startSquares = for (square <- perimeter) yield {
      List(Up, Down, Left, Right)
        .filter(direction => !map.contains(direction.apply(square)))
        .map(direction => (square, direction))
    }
    if (startSquares.length == 0)
      return List(((0, 0), Right)) // ensures the first move goes somewhere
    else
      return startSquares.flatten
  }

  override def toString: String = {
    val (xs, ys) = map.keys.unzip
    val (maxX, maxY) = (xs.max + 1, ys.max + 1)
    val (minX, minY) = (xs.min - 1, ys.min - 1)

    (for (y <- minY to maxY) yield {
      (for (x <- minX to maxX) yield map.get((x, y)) match {
        case Some(piece) => piece.toString()
        case None => ".."
      }).mkString(" ")
    }).mkString("\n")
  }
}

case class GameState(
  board: Board,
  bag: List[Piece],
  playerBags: List[List[Piece]],
  turn: Int) {

  private def currentBag(): List[Piece] = playerBags(turn)

  def generateMoves(): List[Move] = {
    var moves = List[Move]()

    def subLists[A](list: List[A]): List[List[A]] = (1 to list.length).flatMap(list.combinations(_)).toList
    def subPermutations[A](lists: List[List[A]]) = lists.flatMap(_.permutations).toList

    // get possible start squares and directions
    board.getStartConfigurations().foreach {
      case (square, direction) =>
        val possiblePlays = subPermutations(subLists(this.currentBag()))
        possiblePlays.foreach { listOfPieces =>
          moves = PlacePieces(square, direction, listOfPieces) :: moves
        }
    }

    return moves.filter(board.allowsMove(_)) :+ SwapPieces
  }

  private def playerBagFunction(function: List[Piece] => List[Piece]): List[List[Piece]] = {
    this.playerBags.take(turn) ++ List(function(currentBag())) ++ this.playerBags.drop(turn + 1)
  }

  def applyMove(move: Move): GameState = move match {
    case PlacePieces(square, direction, listOfPieces) => {
      val newBoard = board.put(square, direction, listOfPieces)

      // remove pieces from the appropriate bag
      var newBag = bag
      val newPlayerBags = playerBagFunction { playerBag =>
        val (reducedBag, newPlayerBag) = Utils.resupplyPlayerBag(bag, playerBag.diff(listOfPieces), 6)
        newBag = reducedBag
        newPlayerBag
      }
      return new GameState(newBoard, newBag, newPlayerBags, (turn + 1) % playerBags.length)
    }
    case SwapPieces => throw new UnsupportedOperationException("not implemented")
  }

}

