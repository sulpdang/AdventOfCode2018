//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer,Map => MM}
import myutil._

object Main extends Day(15) {

  type Group      = Seq[Monster]
  type MonsterPos = MM[Pos, Monster]
  type Dungeon    = Array[Array[Char]]
  type Input      = Dungeon

  implicit class DungeonUtil(val arr:Dungeon) extends AnyVal {
    def apply(pos:Pos)    = arr(pos.y)(pos.x)
    def isElf(pos:Pos)    = arr(pos) == 'E'
    def isGoblin(pos:Pos) = arr(pos) == 'G'
    def canWalk(pos:Pos)  = arr(pos) == '.'
    def in(pos:Pos)   = {
      pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height
    }
    def height = arr(0).length
    def width  = arr.length
    def update(pos:Pos, value:Char) { arr(pos.y)(pos.x) = value }
    def print = arr.map{_.mkString("")}.mkString("\n")
  }

  implicit class PosUtil(val pos:Pos) extends AnyVal {
    def nears(dungeon:Dungeon):List[Pos] = List(Pos(0, -1), Pos(-1, 0), Pos(1, 0), Pos(0, 1))
      .map{ pos + _ }.filter{dungeon.in(_)}
  }
  case class Path(pos:Pos, prev:Option[Path] = None) {
    def root:Path = prev match {
      case Some(parent) => parent.root
      case None => this
    }
  }

  case class Monster(var pos:Pos, val isGoblin:Boolean, val attackPower:Int, var hp:Int=200) extends Ordered[Monster] {

    def isElf = !isGoblin
    def isDead  = hp <= 0
    def isAlive = !isDead
    def nextWalk(dungeon:Dungeon):List[Pos] =
      pos.nears(dungeon).filter{dungeon.canWalk(_)}
    def nearEnemy(dungeon:Dungeon, posInfo:MonsterPos) = {
      pos.nears(dungeon).collect {
        case pos if isGoblin && dungeon.isElf(pos) => (posInfo(pos), pos)
        case pos if !isGoblin && dungeon.isGoblin(pos) => (posInfo(pos), pos)
      }.sorted.map{_._1}.headOption
    }

    def isEnemy(other:Monster) = this.isGoblin != other.isGoblin
    def damaged { 
      hp -= (if(isElf) 3 else attackPower)
    }

    def compare(other:Monster) = {
      if(other.hp != this.hp) this.hp - other.hp
      else this.pos.compare(other.pos)
    }
  }

  case class State(goblins:Group, elves:Group, dungeon:Dungeon, posInfo:MonsterPos, val round:Int = 0) {
    def next:State = {
      @tailrec
      def roundAcc(remain:Group, goblins:Group, elves:Group, dungeon:Dungeon, posInfo:MonsterPos):State = {
        remain match {
          case Nil => State(goblins, elves, dungeon, posInfo, round+1)
          case Seq(head, tail @ _*) if head.isDead => roundAcc(tail, goblins, elves, dungeon, posInfo)
          case Seq(head, tail @ _*) => {
            require(head.isAlive)
            head.nearEnemy(dungeon, posInfo) match {
              case None => {
                val enemies = if(head.isGoblin) elves else goblins
                enemies.flatMap{_.nextWalk(dungeon)} match {
                  case Nil =>
                  case goals =>
                    val nextTargetIter = genPosIter(dungeon, head.nextWalk(dungeon))
                      .flatMap{_._1}.dropWhile{path => !goals.exists(_ == path.pos)}

                    if(nextTargetIter.hasNext) {
                      val nextTargetPath = nextTargetIter.next
                      val nextMove = nextTargetPath.root.pos
                      dungeon(head.pos) = '.'
                      posInfo -= head.pos

                      head.pos = nextMove
                      val nextValue = if(head.isGoblin) 'G' else 'E'

                      dungeon(nextMove) = nextValue
                      posInfo(head.pos) = head
                    }
                }
              }
              case _ =>
            }
            head.nearEnemy(dungeon, posInfo) match {
              case Some(nearEnemy) =>
                nearEnemy.damaged
                if(nearEnemy.isDead) {
                  val deadPos = nearEnemy.pos
                  dungeon(deadPos) = '.'
                  if(nearEnemy.isGoblin)
                    roundAcc(tail, goblins.filterNot{_ == nearEnemy}, elves, dungeon, posInfo - deadPos)
                  else
                    roundAcc(tail, goblins, elves.filterNot{_ == nearEnemy}, dungeon, posInfo - deadPos)

                } else roundAcc(tail, goblins, elves, dungeon, posInfo)

              case None => roundAcc(tail, goblins, elves, dungeon, posInfo)
            }
          }
        }
      }
      roundAcc(units.sortBy{_.pos}, goblins, elves, dungeon, posInfo)
    }
    def units = goblins ++ elves
    def isEnd:Boolean = elves.length == 0 || goblins.length == 0
  }
  def genPosIter(dungeon:Dungeon, nexts:List[Pos]) = {
    Iterator.iterate((nexts.map{Path(_)}, nexts.toSet)) {
      case (pathes, totVisit) =>
        pathes.foldLeft((List[Path](), totVisit)){ case ((res, visited), path) =>
          val nextPoses = path.pos.nears(dungeon)
                .filter{pos => dungeon.canWalk(pos) && !visited.contains(pos)}
          (res ++ nextPoses.map{pos=>Path(pos, Some(path))}, visited ++ nextPoses)
        }
    }.takeWhile(!_._1.isEmpty)
  }

  object State {
    def make(dungeon:Array[Array[Char]], attackPower:Int=3):State = {
      val units = for{
        y <- (0 until dungeon.height)
        x <- (0 until dungeon.width)
        c = dungeon(y)(x)
        if c == 'G' || c == 'E'
      } yield Monster(Pos(x, y), c == 'G', attackPower)
      val monsterPos = MM(units.map{x=>x.pos -> x}: _*)
      val (goblins, elves) = units.partition{_.isGoblin}
      State(goblins, elves, dungeon, monsterPos)
    }
  }

  def processedInput = input.map{_.trim}.map{_.toCharArray}.toArray

  def makeValue(input:Input, power:Int):(State, State, Int) = {
    val state = State.make(input, power)
    val endState = Iterator.iterate(state){_.next}
      .dropWhile(!_.isEnd)
      .next
    (state, endState, endState.units.map{_.hp}.sum * (endState.round-1))
  }
  def solve(input:Input) = makeValue(input, 3)._3
  def solve2(input:Input) = {
    @tailrec
    def findAlive(start:Int=1, end:Int=200, res:Int=0):Int = {
      if(end-start <= 1) res 
      else {
        val mid = (end + start)/2
        val (state, endState, value) = makeValue(input.map{_.clone}, mid)
        if(state.elves.length != endState.elves.length) findAlive(mid, end, res)
        else findAlive(start, mid, value)
      }
    }
    findAlive()
  }
}
