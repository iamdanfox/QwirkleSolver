package quirkle

case class GameState(
  board: Board,
  playerBags: Array[List[Piece]],
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
        val possiblePlays = subPermutations(subLists(this.currentBag()))  // SLOW
        possiblePlays.foreach { listOfPieces =>
          moves = PlacePieces(square, direction, listOfPieces) :: moves
        }
    }

    //PROFILER SAYS 82% of time spent in here
    val legalMoves = moves.filter(board.allowsMove(_))

    if (bag.length > 0)
      return legalMoves :+ SwapPieces
    else
      return legalMoves
  }

  private def playerBagFunction(function: List[Piece] => List[Piece]): Array[List[Piece]] = {
    val newPlayerBags = playerBags.clone()
    newPlayerBags(turn) = function(newPlayerBags(turn))
    return newPlayerBags
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

