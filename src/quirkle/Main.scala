package quirkle

object Main {
  def main(args: Array[String]): Unit = {
    var gameState = GameState.makeInitialGameState(2)
    //    Console.readLine()
    do {
      val moves = gameState.generateMoves()
      println("Player " + gameState.turn + " turn (score=" + gameState.currentPlayer().score +
        ", currentBag:" + gameState.currentPlayer().playerBag + " (" + moves.length + " possible moves)")
      Console.readLine()
      gameState = gameState.applyMove(moves(0))
      println(gameState.board + "\n\n")
    } while (true)
  }
}