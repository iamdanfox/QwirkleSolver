package quirkle

object Main {

  def main(args: Array[String]): Unit = {

    val initialMap = new scala.collection.mutable.HashMap[(Int, Int), Piece]()
    initialMap.put((0, 0), Piece(Red, Square))
    
    val board = new Board(initialMap)
    
    val w = Piece(Red,Square)
    val x = Piece(Red,Circle)
    val y = Piece(Red,Spiky)
    val z = Piece(Red,Cross)
    
    val gameState = GameState(board, List(w), List(List(w, x), List(y, z)), 0)
    val moves = gameState.generateMoves()
    moves.foreach(println(_))
    println()
    println(gameState.applyMove(moves(5)))
  }

}