package qwirkle

object Main {
  def main(args: Array[String]): Unit = {

    var done = false
    var gameState = GameState.makeInitialGameState(2)

    Console.readLine()
    
    do {
      val moves = gameState.generateMoves()
      println("Player " + gameState.turn + " turn (score=" + gameState.currentPlayer().score +
        ", currentBag:" + gameState.currentPlayer().playerBag + " (" + moves.length + " possible moves)")

      if (moves.length > 0) {

        val chosenMove = gameState.turn match {
          case 0 => greedy(moves, gameState)
          case 1 => greedy(moves, gameState)
        }

        gameState = gameState.applyMove(chosenMove)
        println(gameState.board + "\n\n")

      } else {
        done = true
      }
    } while (!done)
      
    val scores = gameState.players.map(playerState => playerState.score)
    println("Game finished, scores: " + scores.mkString(", ") + " total=" + scores.sum)
  }

  def greedy(moves: List[Move], gameState: GameState): Move = {
    moves.maxBy(move => gameState.board.scoreMove(move))
  }

  def greedyPreferTurnover(moves: List[Move], gameState: GameState): Move = {
    val scoreMapping = moves.groupBy(move => gameState.board.scoreMove(move))
    val bestScoringMoves = scoreMapping(scoreMapping.keys.max)
    bestScoringMoves.maxBy(move => move match {
      case PlacePieces(_, _, pieces) => pieces.length
      case SwapPieces => 0
    })
  }

  def greedyAvoidTurnover(moves: List[Move], gameState: GameState): Move = {
    val scoreMapping = moves.groupBy(move => gameState.board.scoreMove(move))
    val bestScoringMoves = scoreMapping(scoreMapping.keys.max)
    bestScoringMoves.minBy(move => move match {
      case PlacePieces(_, _, pieces) => pieces.length
      case SwapPieces => Int.MaxValue
    })
  }

  def maxPieceValue(moves: List[Move], gameState: GameState): Move = {
    moves.maxBy(move => move match {
      case PlacePieces(_, _, pieces) => gameState.board.scoreMove(move) / pieces.length
      case SwapPieces => 0
    })
  }

  def alternateGreedy(moves: List[Move], gameState: GameState): Move = {
    wasGreedyLastTime = !wasGreedyLastTime
    if (wasGreedyLastTime) maxPieceValue(moves, gameState) else greedy(moves, gameState)
  }
  var wasGreedyLastTime = false

}