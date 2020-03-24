//
// Main
//
// Copyright (c) 2018 Yohho (sulpdang@gmail.com)
//

package solver

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MM}
import myutil._
import scala.collection.immutable.SortedSet

object Main2 extends Day(15) {

  type Input = List[String]

  trait Elem
  case object Wall extends Elem
  trait Monster extends Elem with Ordered[Monster] {
    var pos:Pos

    var hp:Int = 200
    def see(other:Monster) = pos.nears.contains{other.pos}
    def damaged(powerAttacked:Int, world:World):World = {
      this.hp -= powerAttacked
      if(this.isDead) { World(world.map - pos) } else world
    }
    def isAlive = !isDead
    def isDead = hp <= 0
    def isGoblin:Boolean
    def isEnemy(t:Monster):Boolean = (t.isGoblin ^ isGoblin)
    def nears(world:World):List[Pos] = this.pos.nears.filter{world.canWalk}
    def compare(other:Monster) = this.pos.compare(other.pos)
  }

  case class World(map:Map[Pos,Elem]) {
    def canWalk(pos:Pos) = !map.contains(pos)
    def move(monster:Monster, nextPos: => Pos):World = {
      val pos = monster.pos
      monster.pos = nextPos
      this.copy(map = (map - pos) + (nextPos -> monster))
    }
  }

  case class Elf(var pos:Pos) extends Monster { def isGoblin = false }
  case class Goblin(var pos:Pos) extends Monster { def isGoblin = true }
  case class Path(pos:Pos) {
    def ->(child:Path):Path = { child.prev = Some(this); child }
    var prev:Option[Path] = None
    def root:Path = prev.map{_.root}.getOrElse(this)
  }
  case class State(monster:SortedSet[Monster], world:World, elfPower:Int=3) extends Itera{
    def isEnd = {
      lazy val check = monster.map{_.isGoblin} 
      (monster.size <= 1) || (check.forall{x=>x} || check.forall{x => !x})
    }

    def move(head:Monster, enemies:SortedSet[Monster], worldAcc:World) = {
        val next = head.nears(worldAcc)
        val goals = enemies.flatMap{_.nears(worldAcc)}
        Iterator.iterate((next.map{Path(_)}, next.toSet)){ case (pathes, visited)=>
          pathes.foldLeft(List.empty[Path], visited) { case ((res, visitedAcc), path) => {
            val nextPoses = path.pos.nears
              .filter{pos => worldAcc.canWalk(pos) && !visitedAcc.contains(pos)}
            (res ++ nextPoses.map{path -> Path(_)}, visitedAcc ++ nextPoses)
          }}
        }.takeWhile(!_._1.isEmpty).flatMap{_._1}
        .dropWhile{path => !goals.exists(_ == path.pos)}
        .toStream.headOption.map{ nextPath => 
          worldAcc.move(head, nextPath.root.pos)
        }.getOrElse(worldAcc)
    }

    def next:State = {
      @tailrec
      def nextAcc(remain:SortedSet[Monster], processed:SortedSet[Monster] = SortedSet(), worldAcc:World):State = {
        val (goblinsAcc, elvesAcc) = (processed ++ remain).filter{_.isAlive}.partition{_.isGoblin}
        remain match {
          case s if s.isEmpty => this.copy(monster=processed.filter{_.isAlive}, world = worldAcc)
          case s => {
            val (head, tail) = (s.head, s.tail)
            val enemies = if(head.isGoblin) elvesAcc else goblinsAcc
            def nearEnemies = enemies.filter{head.see}
            val movedWorld = if(nearEnemies.isEmpty) { move(head, enemies, worldAcc) } else worldAcc
            val resultWorld = if (nearEnemies.isEmpty) movedWorld
            else nearEnemies.minBy{_.hp}.damaged(if(head.isGoblin) 3 else elfPower, movedWorld)
            nextAcc(tail.filter{_.isAlive}, {processed + head}.filter{_.isAlive}, resultWorld)
          }
        }
      }
      nextAcc(monster.filter{_.isAlive}, worldAcc = world)
    }
  }

  def processedInput = input
  object State {
    def make(input:Input, attackPower:Int) = {
      val prevMap = Area(Pos(0,0), Pos(input(0).length-1, input.length-1)).points
        .map{pos => (pos, input(pos.y)(pos.x))}.collect{
          case (pos, '#') => pos -> Wall
          case (pos, 'G') => pos -> Goblin(pos)
          case (pos, 'E') => pos -> Elf(pos)
        }.toSeq
      val world = World(Map(prevMap:_*))
      val monsterSeq = prevMap.map{_._2}.collect{case x:Monster => x}
      State(SortedSet(monsterSeq: _*), world, elfPower = attackPower)
    }
  }

  def madeSolution(prevMap:Input, attackPower:Int = 3) = {
    val state = State.make(prevMap, attackPower)
    lazy val stateStream:Stream[State] = state #:: stateStream.map{_.next}
    val (last, round) = stateStream.zip{Stream.from(0)}.dropWhile{!_._1.isEnd}.head
    ((round-1) * last.monster.toList.filter{_.isAlive}.map{_.hp}.sum, last)
  }

  def solve(input:Input) = madeSolution(input)._1
  def solve2(input:Input) = {
    val numOfElves = input.map{_.count{_ == 'E'}}.sum
    @tailrec
    def findElfPower(lo:Int=1, hi:Int=201, res:Int=0):Int = {
      if(hi - lo <= 1) res
      else {
        val mid = (lo + hi)/2
        val (value, state) = madeSolution(input, mid)
        val isElfDead = state.monster.filter{!_.isGoblin}.size != numOfElves
        if (isElfDead) findElfPower(mid, hi, res)
        else findElfPower(lo, mid, value)
      }
    }
    findElfPower()
  }

}
