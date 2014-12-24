package quirkle

object Main {
  def main(args: Array[String]): Unit = {
    
    val b = new ABoard(Array.ofDim[Piece](40,40), 20)
    
    
    var gameState = GameState.makeInitialGameState(2)
    do {
      val moves = gameState.generateMoves()
      println("Player " + gameState.turn + " turn (score=" + gameState.currentPlayer().score +
        ", currentBag:" + gameState.currentPlayer().playerBag + " (" + moves.length + " possible moves)")
//      Console.readLine()
      
      val bestmove = moves.maxBy(move => gameState.applyMove(move).board.scoreLastMove(move))
      
      gameState = gameState.applyMove(bestmove)
      println(gameState.board + "\n\n")
    } while (true)
  }
}