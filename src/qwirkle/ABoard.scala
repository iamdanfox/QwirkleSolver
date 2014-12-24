package qwirkle


object ABoard {
  val DIM = 50 
  def makeBlank(): Board = new ABoard(Array.ofDim[Piece](2*DIM,2*DIM), DIM)
}

case class ABoard(private val array: Array[Array[Piece]], private val DIM: Int) extends Board {

  def put(square: (Int, Int), direction: Direction, listOfPieces: List[Piece]): ABoard = {
    val newArray = array.clone()
    for (i <- 0 until 2*DIM) newArray(i) = array(i).clone() // deep copy
      
    direction.applyStream(square).zip(listOfPieces).foreach { case ((x,y),piece) => newArray(x+DIM)(y+DIM) = piece }
    return new ABoard(newArray, DIM);
  }

  def contains(square: (Int,Int)): Boolean = array(square._1+DIM)(square._2+DIM) != null
  
  def get(square: (Int,Int)): Option[Piece] = {
    val rtn = apply(square)
    if (rtn != null) Some(rtn) else None
  }
  
  def apply(square: (Int, Int)): Piece = array(square._1+DIM)(square._2+DIM)
  
  def keys: Iterable[(Int,Int)] = {
    for (x <- -DIM until DIM; y <- -DIM until DIM; if contains((x,y))) yield (x,y)
  }

  protected override def piecesInDirection(square: (Int, Int), direction: Direction): List[Piece] = {
    val oneStep = direction.apply(square)
    if (this.contains(oneStep)) 
      return this.apply(oneStep) :: piecesInDirection(oneStep, direction)
    else
      return List()
  }
  
}