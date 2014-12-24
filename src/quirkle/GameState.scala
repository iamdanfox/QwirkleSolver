package quirkle

case class Piece(colour: Colour, shape: Shape) {
  override def toString: String = colour.toString() + shape.toString()
}

object Utils {
  def getNeighbours(square: (Int, Int)): List[(Int, Int)] =
    List((square._1, square._2 + 1),
      (square._1 + 1, square._2 + 1),
      (square._1 + 1, square._2),
      (square._1 + 1, square._2 - 1),
      (square._1, square._2 - 1),
      (square._1 - 1, square._2 - 1),
      (square._1 - 1, square._2),
      (square._1 - 1, square._2 + 1))

  def resupplyPlayerBag(bag: List[Piece], playerBag: List[Piece], playerNumber: Int): List[Piece] = {
    playerBag ++ scala.util.Random.shuffle(bag).take(playerNumber - playerBag.length)
  }
}

trait Move {}
case class PlacePieces(startSquare: (Int, Int), direction: Direction, pieces: List[Piece]) extends Move
object SwapPieces extends Move

class Board( final val map: scala.collection.Map[(Int, Int), Piece]) {

  def put(location: (Int, Int), piece: Piece): Board = new Board(map.+((location, piece)))

  def getPerimeter(): List[(Int, Int)] = {
    var perimeter = List[(Int, Int)]()
    map.foreach {
      case (square, _) =>
        perimeter = perimeter ++ Utils.getNeighbours(square).filter { !map.contains(_) }
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

    // TODO: filter out legal moves!

    return moves
  }

  private def playerBagFunction(function: List[Piece] => List[Piece]): List[List[Piece]] = {
    this.playerBags.take(turn) ++ List(function(currentBag())) ++ this.playerBags.drop(turn + 1)
  }

  def applyMove(move: Move): GameState = move match {
    case PlacePieces(square, direction, listOfPieces) => {
      // put pieces on the board
      var list = listOfPieces
      var currentSquare = square
      var board = this.board
      while (list != Nil) {
        val piece :: tail = list
        board = board.put(currentSquare, piece)
        currentSquare = direction.apply(currentSquare)
        list = tail
      }

      // remove from the appropriate bag
      val newPlayerBags = playerBagFunction { playerBag =>
        playerBag.diff(listOfPieces)
      }
      return new GameState(board, bag, newPlayerBags, (turn + 1) % playerBags.length)
    }
    case SwapPieces => throw new UnsupportedOperationException("not implemented")
  }

}

