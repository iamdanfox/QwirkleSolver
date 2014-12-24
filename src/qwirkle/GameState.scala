package qwirkle

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

    return new GameState(ABoard.makeBlank(), playerBags.map(bag => PlayerState(bag, 0)), bag, 0)
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

    // get possible start squares and directions
    val playerBag = currentPlayer().playerBag
    board.getStartConfigurations().foreach {
      case (square, direction) =>

        val piecesQueue = new scala.collection.mutable.Queue[List[Piece]]
        piecesQueue ++= playerBag.combinations(1)

        while (!piecesQueue.isEmpty) {
          val list = piecesQueue.dequeue()
          val move = PlacePieces(square, direction, list)
          if (board.allowsMove(move)){
            moves = move :: moves
            piecesQueue ++= playerBag.diff(list).combinations(1).map(piece => list ++ piece)
          }
        }
    }

    if (bag.length > 0)
      return moves :+ SwapPieces
    else
      return moves
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

          val lastMoveBonus = if (newBag.length == 0 && newPlayerBag.length == 0) 6 else 0
          PlayerState(newPlayerBag, score + newBoard.scoreLastMove(placeMove) + lastMoveBonus)
      }

      return new GameState(newBoard, newPlayerBags, newBag, (turn + 1) % players.length)
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

