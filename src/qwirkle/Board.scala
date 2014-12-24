package qwirkle

trait Board {
  def put(square: (Int, Int), direction: Direction, listOfPieces: List[Piece]): Board

  protected def contains(square: (Int, Int)): Boolean
  protected def apply(square: (Int, Int)): Piece
  protected def keys: Iterable[(Int, Int)]
  protected def get(square: (Int, Int)): Option[Piece]

  /**
   * This CAN be a prospective move! (pieces don't have to be laid down yet)
   */
  def scoreMove(move: Move): Int = move match {
    case PlacePieces(square, direction, listOfPieces) => {
      val newLines = getNewlyFormedLines(square, direction, listOfPieces).filter(_.length > 1)
      return newLines.map(_.length).sum + (newLines.filter(_.length == 6).length * 6)
    }
    case SwapPieces => 0
  }

  def allowsMove(move: Move): Boolean = move match {
    case PlacePieces(startSquare, direction, pieces) => {
      val squares = direction.applyStream(startSquare).take(pieces.length)

      // rules:
      // piece must have come from player's bag (implicit - not enforced)
      // pieces must be placed in one line (implicit - not enforced)

      // mustn't place pieces on already occupied space
      for (square <- squares)
        if (this.contains(square))
          return false

      for (line <- getNewlyFormedLines(startSquare, direction, pieces)) {
        // no repeated pieces allowed
        if (line.distinct.length != line.length) // TODO: make distinct faster!
          return false

        // pieces must form a line of one color/shape
        if (line.map(_.colour).distinct.length > 1 && line.map(_.shape).distinct.length > 1)
          return false
      }

      return true
    }
    case SwapPieces => true
  }

  /*
   * NB. this doesn't perform any copying or mutate any state :)
   */
  protected def getNewlyFormedLines(startSquare: (Int, Int), direction: Direction, pieces: List[Piece]): Stream[List[Piece]] = {
    val squares = direction.applyStream(startSquare).take(pieces.length)

    val mainLine = piecesInDirection(startSquare, direction.opposite) ++ pieces ++ piecesInDirection(squares.last, direction)
    val perpendicularLines = if (direction == Up || direction == Down)
      squares.zip(pieces).map(element => this.hLine(element._1,element._2))
    else
      squares.zip(pieces).map(element => this.vLine(element._1,element._2))

    return mainLine #:: perpendicularLines
  }

  protected def piecesInDirection(square: (Int, Int), direction: Direction): List[Piece] =
    return direction.applyStream(square).drop(1).takeWhile(this.contains(_)).map(this.apply(_)).toList

  // NB. the Left and Up components are returned in reverse order, this doesn't compromise validation
  protected def hLine(square: (Int, Int), piece: Piece): List[Piece] =
    return piecesInDirection(square, Left) ++ List(piece) ++ piecesInDirection(square, Right)

  protected def vLine(square: (Int, Int), piece: Piece): List[Piece] =
    return piecesInDirection(square, Up) ++ List(piece) ++ piecesInDirection(square, Down)

  def getPerimeter(): List[(Int, Int)] = {
    var perimeter = List[(Int, Int)]()

    def getNeighbours(square: (Int, Int)): List[(Int, Int)] =
      List(Up, Down, Left, Right).map(_.apply(square))

    this.keys.foreach { square =>
      perimeter = perimeter ++ getNeighbours(square).filter { !this.contains(_) }
    }
    return perimeter
  }

  def getStartConfigurations(): List[((Int, Int), Direction)] = {
    val perimeter = getPerimeter()
    val startSquares = for (square <- perimeter) yield {
      List(Up, Down, Left, Right)
        .filter(direction => !this.contains(direction.apply(square)))
        .map(direction => (square, direction))
    }
    if (startSquares.length == 0)
      return List(((0, 0), Right)) // ensures the first move goes somewhere
    else
      return startSquares.flatten
  }

  override def toString: String = {
    val (xs, ys) = this.keys.unzip
    val (maxX, maxY, minX, minY) = if (this.keys.isEmpty)
      (1, 1, -1, -1)
    else
      (xs.max + 1, ys.max + 1, xs.min - 1, ys.min - 1)

    (for (y <- minY to maxY) yield {
      (for (x <- minX to maxX) yield this.get((x, y)) match {
        case Some(piece) => piece.toString()
        case None => ".."
      }).mkString(" ")
    }).mkString("\n")
  }
}
