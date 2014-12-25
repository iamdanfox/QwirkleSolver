package qwirkle

package qwirkle

object ABoard1 {
  val DIM = 50
  def makeBlank(): Board = new ABoard1(Array.ofDim[Piece](2 * DIM * 2 * DIM), DIM)
}

case class ABoard1(private val array: Array[Piece], private val DIM: Int) extends Board {

  def put(square: (Int, Int), direction: Direction, listOfPieces: List[Piece]): ABoard1 = {
    val newArray = array.clone()

    direction.applyStream(square).zip(listOfPieces).foreach { case ((x, y), piece) => newArray(2 * DIM * (x + DIM) + y + DIM) = piece }
    return new ABoard1(newArray, DIM);
  }

  def contains(square: (Int, Int)): Boolean = array(2 * DIM * (square._1 + DIM) + square._2 + DIM) != null

  def get(square: (Int, Int)): Option[Piece] = {
    val rtn = apply(square)
    if (rtn != null) Some(rtn) else None
  }

  def apply(square: (Int, Int)): Piece = array(2 * DIM * (square._1 + DIM) + square._2 + DIM)

  def keys: Iterable[(Int, Int)] = {
    for (i <- 0 until 2 * DIM * 2 * DIM; if array(i) != null) yield ((i / (2 * DIM)) - DIM, (i % (2 * DIM)) - DIM)
  }

  protected override def piecesInDirection(square: (Int, Int), direction: Direction): List[Piece] = {
    val oneStep = direction.apply(square)
    if (this.contains(oneStep))
      return this.apply(oneStep) :: piecesInDirection(oneStep, direction)
    else
      return List()
  }
}