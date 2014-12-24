package quirkle


object HBoard {
  def makeBlank(): Board = new HBoard(new scala.collection.immutable.HashMap[(Int, Int), Piece])
}

case class HBoard(private val map: scala.collection.immutable.Map[(Int, Int), Piece]) extends Board {

  def put(square: (Int, Int), direction: Direction, listOfPieces: List[Piece]): HBoard = {
    val newMap = this.map ++ direction.applyStream(square).zip(listOfPieces)
    return new HBoard(newMap)
  }

  protected def contains(square: (Int,Int)): Boolean = this.map.contains(square)
  
  protected def get(square: (Int,Int)): Option[Piece] = map.get(square)
  
  protected def apply(square: (Int, Int)): Piece = map.apply(square)
  
  protected def keys: Iterable[(Int,Int)] = map.keys

}