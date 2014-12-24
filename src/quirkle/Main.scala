package quirkle

object Main {
  def main(args: Array[String]): Unit = {
    var gameState = Utils.makeInitialGameState(2)
//    Console.readLine()
    do {
      val moves = gameState.generateMoves()
      println("Player " + gameState.turn + " turn, currentBag:" + gameState.currentBag + " (" + moves.length + " possible moves)")
      Console.readLine()
      gameState = gameState.applyMove(moves(0))
      println(gameState.board + "\n\n")
    } while (true)
  }
}