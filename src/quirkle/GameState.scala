package quirkle

case class Piece(colour: Colour, shape: Shape) {
  override def toString: String = colour.toString() + shape.toString()
}

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

    return new GameState(board, playerBags, bag, 0)
  }
}

trait Move {}
case class PlacePieces(startSquare: (Int, Int), direction: Direction, pieces: List[Piece]) extends Move
object SwapPieces extends Move

case class Board(map: scala.collection.immutable.Map[(Int, Int), Piece]) {

  def put(square: (Int, Int), direction: Direction, listOfPieces: List[Piece]): Board = {
    val newMap = this.map ++ direction.applyStream(square).zip(listOfPieces)
    return new Board(newMap)
  }

  def allowsMove(move: Move): Boolean = move match {
    case PlacePieces(startSquare, direction, pieces) => {
      val squares = direction.applyStream(startSquare).take(pieces.length)

      // rules:
      // piece must have come from player's bag (implicit - not enforced)
      // pieces must be placed in one line (implicit - not enforced)

      // mustn't place pieces on already occupied space
      for (square <- squares)
        if (this.map.contains(square))
          return false

      for (line <- getNewlyFormedLines(startSquare, direction, pieces)) {
        // no repeated pieces allowed
        if (line.distinct.length != line.length)
          return false

        // pieces must form a line of one color/shape
        if (line.map(_.colour).distinct.length > 1 && line.map(_.shape).distinct.length > 1)
          return false
      }

      return true
    }
    case SwapPieces => true
  }

  protected def getNewlyFormedLines(startSquare: (Int, Int), direction: Direction, pieces: List[Piece]): List[List[Piece]] = {
    // place pieces onto a copy of board (immutability important here!) to test other rules
    val board2 = this.put(startSquare, direction, pieces)

    val mainLine = if (direction == Up || direction == Down) board2.vLine(startSquare) else board2.hLine(startSquare)
    val squares = direction.applyStream(startSquare).take(pieces.length).toList
    val perpendicularLines = if (direction == Up || direction == Down) squares.map(board2.hLine(_)) else squares.map(board2.vLine(_))

    return mainLine :: perpendicularLines
  }

  protected def piecesInDirection(square: (Int, Int), direction: Direction): List[Piece] =
    return direction.applyStream(square).drop(1).takeWhile(map.contains(_)).map(map.apply(_)).toList

  // NB. the Left and Up components are returned in reverse order, this doesn't compromise validation
  protected def hLine(square: (Int, Int)): List[Piece] =
    return piecesInDirection(square, Left) ++ List(map(square)) ++ piecesInDirection(square, Right)

  protected def vLine(square: (Int, Int)): List[Piece] =
    return piecesInDirection(square, Up) ++ List(map(square)) ++ piecesInDirection(square, Down)

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
    val (maxX, maxY, minX, minY) = if (map.keys.isEmpty)
      (1, 1, -1, -1)
    else
      (xs.max + 1, ys.max + 1, xs.min - 1, ys.min - 1)

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
  playerBags: List[List[Piece]],
  bag: List[Piece],
  turn: Int) {

  def currentBag(): List[Piece] = playerBags(turn)

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
        val (reducedBag, newPlayerBag) = Utils.resupplyPlayerBag(bag, playerBag.diff(listOfPieces))
        newBag = reducedBag
        newPlayerBag
      }
      return new GameState(newBoard, newPlayerBags, newBag, (turn + 1) % playerBags.length)
    }
    case SwapPieces => throw new UnsupportedOperationException("not implemented")
  }

}

