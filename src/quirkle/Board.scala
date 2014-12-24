package quirkle

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

  // PROFILER SAYS 62% of time spent here. IDEA. store the lines already?
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