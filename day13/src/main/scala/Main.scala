//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import myutil._

object Main extends Day(13) {

  abstract class Direction(value:Int, nextAdded:Pos) {
    def +(added:Int):Direction = directionList((value+added) % 4)
    def +(pos:Pos) = pos + nextAdded
    def toInt = value
  }

  case object Left  extends Direction(0, Pos(-1, 0))
  case object Down  extends Direction(1, Pos(0, 1))
  case object Right extends Direction(2, Pos(1, 0))
  case object Up    extends Direction(3, Pos(0, -1))

  private lazy val directionList:Array[Direction] = Array(Left, Down, Right, Up)
  private lazy val turnList = List(1, 0, 3)
  private lazy val nextDirection = Map[Char, Array[Direction]](
    '/'  -> Array(Down, Left, Up, Right), '\\'  -> Array(Up, Right, Down, Left)
  )
  private lazy val charToDirection = Map[Char, Direction](
    '^' -> Up, 'v' -> Down, '<' -> Left, '>' -> Right
  )

  case class Cart(dir:Direction, pos:Pos, curTurn:Int=0) {

    def move:Cart = {
      val nextPos = dir + pos
      val (nextDir:Direction, nextTurn) = roadMap(nextPos.y)(nextPos.x) match {
        case '/'  => (nextDirection('/')(dir.toInt), curTurn)
        case '\\' => (nextDirection('\\')(dir.toInt), curTurn)
        case '+'  => (dir + turnList(curTurn), (curTurn + 1) % 3)
        case _    => (dir, curTurn)
      }
      Cart(nextDir, nextPos, nextTurn)
    }
  }

  type Input = Iterator[(List[Cart], List[Cart])]

  def processedInput = {
    implicit val ordering:Ordering[Cart] = Ordering.by{case Cart(_, Pos(x, y), _) => (x, y)}
    roadMap = input.map{_.toCharArray}.toArray

    val carts = for {
      y <- (0 until input.length).toList
      x <- 0 until input(0).length
      c = roadMap(y)(x)
      if charToDirection.keySet contains c
    } yield Cart(charToDirection(c), Pos(x, y))

    Iterator.iterate[(List[Cart], List[Cart])]((carts.sorted, List[Cart]())) {
      case (Nil, list) => (list.sorted, Nil)
      case (head::tail, remain) =>
        val moved = head.move
        (tail++remain).find{_.pos == moved.pos} match {
          case Some(crashed) =>
            (tail.filter{_ != crashed}, remain.filter{_ != crashed})
          case None =>
            (tail, remain :+ moved)
        }
    }
  }


  private var roadMap:Array[Array[Char]] = _

  def solve(input:Input) = {
    input.sliding(2)
      .map{case Seq((t, r), (nt, nr))  => (t++r).diff(nt++nr) }
      .collect{ case list@(List(_, crashed)) if list.length >= 2 => crashed.pos }.next
  }

  def solve2(input:Input) = 
    input.collect{ case (Nil, remain) if remain.length == 1 => remain.head.pos } .next

}
