package quirkle

object GameState {

  val NUM_PIECES_PER_PLAYER = 6

  def makeInitialGameState(numPlayers: Int): GameState = {
    val distinctPieces = for (
      color <- List(Red, Orange, Yellow, Green, Blue, Purple);
      shape <- List(Square, Circle, Diamond, Clubs, Spiky, Cross)
    ) yield Piece(color, shape)

    val startBag = scala.util.Random.shuffle(distinctPieces ++ distinctPieces ++ distinctPieces)

    val (playerBags, leftOvers) = startBag.grouped(NUM_PIECES_PER_PLAYER).toList.splitAt(numPlayers)
    val bag = leftOvers.flatten

    val board = new Board(new scala.collection.immutable.HashMap[(Int, Int), Piece])

    return new GameState(board, playerBags.map(bag => PlayerState(bag, 0)), bag, 0)
  }
}

case class PlayerState(playerBag: List[Piece], score: Int)

case class GameState(
  board: Board,
  players: List[PlayerState],
  bag: List[Piece],
  turn: Int) {

  def currentPlayer(): PlayerState = players(turn)

  def generateMoves(): List[Move] = {
    var moves = List[Move]()

    def subLists[A](list: List[A]): List[List[A]] = (1 to list.length).flatMap(list.combinations(_)).toList
    def subPermutations[A](lists: List[List[A]]) = lists.flatMap(_.permutations).toList

    // get possible start squares and directions 
    val possiblePlays = subPermutations(subLists(currentPlayer().playerBag)) // SLOW
    board.getStartConfigurations().foreach {
      case (square, direction) =>
        possiblePlays.foreach { listOfPieces => moves = PlacePieces(square, direction, listOfPieces) :: moves }
    }

    //PROFILER SAYS 82% of time spent in here
    val legalMoves = moves.filter(board.allowsMove(_))

    if (bag.length > 0)
      return legalMoves :+ SwapPieces
    else
      return legalMoves
  }

  private def mutateCurrentPlayerState(function: PlayerState => PlayerState): List[PlayerState] = {
    players.take(turn) ++ List(function(players(turn))) ++ players.drop(turn + 1)
  }

  def applyMove(move: Move): GameState = move match {
    case placeMove @ PlacePieces(square, direction, listOfPieces) => {
      val newBoard = board.put(square, direction, listOfPieces)

      // remove pieces from the appropriate bag
      var newBag = bag
      val newPlayerBags = mutateCurrentPlayerState {
        case PlayerState(playerBag, score) =>
          val (reducedBag, newPlayerBag) = resupplyPlayerBag(bag, playerBag.diff(listOfPieces))
          newBag = reducedBag
          PlayerState(newPlayerBag, score + newBoard.scoreLastMove(placeMove))
      }

      return new GameState(newBoard, newPlayerBags, newBag, (turn + 1) % players.length) // TODO figure out 6 bonus at end of game
    }
    case SwapPieces => {
      var newBag = bag
      val newPlayerBags = mutateCurrentPlayerState {
        case PlayerState(playerBag, score) =>
          val (mainBag, newPlayerBag) = resupplyPlayerBag(bag ++ playerBag, List())
          newBag = mainBag
          PlayerState(newPlayerBag, score)
      }
      return new GameState(board, newPlayerBags, newBag, (turn + 1) % players.length)
    }
  }

  private def resupplyPlayerBag(bag: List[Piece], playerBag: List[Piece]): (List[Piece], List[Piece]) = {
    val takenOutOfBag = scala.util.Random.shuffle(bag).take(GameState.NUM_PIECES_PER_PLAYER - playerBag.length)
    return (bag.diff(takenOutOfBag), playerBag ++ takenOutOfBag)
  }
}

