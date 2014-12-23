

package quirkle

trait Colour
object Red extends Colour
object Green extends Colour
object Blue extends Colour
object Orange extends Colour
object Yellow extends Colour
object Purple extends Colour

trait Shape
object Square extends Shape
object Diamond extends Shape
object Circle extends Shape
object Clubs extends Shape
object Spiky extends Shape
object Cross extends Shape

case class Piece(colour: Colour, shape: Shape)

trait Direction {
  def apply(square: (Int, Int)): (Int, Int)
}
object Up extends Direction {
  def apply(square: (Int, Int)): (Int, Int) = (square._1, square._2 + 1)
}
object Down extends Direction {
  def apply(square: (Int, Int)): (Int, Int) = (square._1, square._2 - 1)
}
object Left extends Direction {
  def apply(square: (Int, Int)): (Int, Int) = (square._1 - 1, square._2)
}
object Right extends Direction {
  def apply(square: (Int, Int)): (Int, Int) = (square._1 + 1, square._2)
}

object Utils {
  def getNeighbours(square: (Int, Int)): List[(Int, Int)] =
    List((square._1, square._2 + 1),
      (square._1 + 1, square._2 + 1),
      (square._1 + 1, square._2),
      (square._1 + 1, square._2 - 1),
      (square._1, square._2 - 1),
      (square._1 - 1, square._2 - 1),
      (square._1 - 1, square._2),
      (square._1 - 1, square._2 + 1))

  def resupplyPlayerBag(bag: List[Piece], playerBag: List[Piece], playerNumber: Int): List[Piece] = {
    playerBag ++ scala.util.Random.shuffle(bag).take(playerNumber - playerBag.length)
  }
}

trait Move {}
case class PlacePieces(startSquare: (Int, Int), direction: Direction, pieces: List[Piece]) extends Move
object SwapPieces extends Move

class Board( final val map: scala.collection.Map[(Int, Int), Piece]) {
  
  def put(location: (Int,Int), piece: Piece): Board = new Board(map.+((location, piece)))

  def getPerimeter(): List[(Int, Int)] = {
    var perimeter = List[(Int, Int)]()
    map.foreach {
      case (square, _) =>
        perimeter = perimeter ++ Utils.getNeighbours(square).filter { !map.contains(_) }
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
}

case class GameState(
  board: Board,
  bag: List[Piece],
  player1bag: List[Piece],
  player2bag: List[Piece],
  player1next: Boolean) {

  private def currentBag(): List[Piece] = if (player1next) player1bag else player2bag

  def generateMoves(): List[Move] = {
    var moves = List[Move]()

    def subLists[A](list: List[A]): List[List[A]] = (1 to list.length).flatMap(list.combinations(_)).toList
    def subPermutations[A](lists: List[List[A]]) = lists.flatMap(_.permutations).toList

    // get possible start squares and directions
    board.getStartConfigurations().foreach {
      case (square, direction) =>
        val possiblePlays = subPermutations(subLists(this.currentBag()))
        possiblePlays.foreach { listOfPieces =>
          moves = PlacePieces(square, direction, listOfPieces) :: moves
        }
    }

    // TODO: filter out legal moves!

    return moves
  }

  def applyMove(move: Move): GameState = move match {
    case PlacePieces(square, direction, listOfPieces) => {
      // put pieces on the board
      var list = listOfPieces
      var currentSquare = square
      var board = this.board
      while (list != Nil) {
        val piece :: tail = list
        board = board.put(currentSquare, piece)
        currentSquare = direction.apply(currentSquare)
        list = tail
      }
      
      // remove from the appropriate bag
      var newPlayer1bag = player1bag
      var newPlayer2bag = player2bag
      if (player1next) {
        newPlayer1bag = newPlayer1bag.diff(listOfPieces)
      } else {
        newPlayer2bag = newPlayer2bag.diff(listOfPieces)
      }
      return new GameState(board, bag, newPlayer1bag, newPlayer2bag, !player1next)
    }
    case SwapPieces => throw new UnsupportedOperationException("not implemented")
  }

}

