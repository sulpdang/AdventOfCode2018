//
// solver.Main
//
// Copyright (c) 2020 Yohho (sulpdang@gmail.com)
//
package solver

import myutil._
import language.implicitConversions

object Main extends Day(24) {
  type Input      = (Family, Family) 
  type Family     = List[Group]
  type AttackType = String

  trait System 
  case object Immune extends System
  case object Infect extends System

  case class Group(var units:Int, hitPoint:Int, attackDamage:Int, damageType:AttackType,
    weak:List[AttackType], immune:List[AttackType], init:Int, system:System) {
      def isAlive = units > 0
      def isEnemy(target:Group) = system != target.system
      def dealtDamage(target:Group) = target match {
        case t if t.weak contains damageType   => effectivePower* 2
        case t if t.immune contains damageType => 0
        case t => effectivePower 
      }
      def effectivePower = attackDamage * units
      def deal(damage:Int) {
        units -= damage / hitPoint 
        if(units < 0) units = 0
      }
    }
  def processedInput = {
    def parseProp(line:String) = {
      if(line == null || line.trim.length == 0) (Nil, Nil)
      else {
        def toList(prop:String) = prop.split(',').map{_.trim}.toList
        def get(arr:Array[Array[String]], idx:Int) = if(arr.size <= idx) Nil else toList(arr(idx)(1))
        val parts = line.drop(1).dropRight(2).split(";").map{_.split("to").map{_.trim}}
        parts(0)(0) match {
          case "weak"   => (get(parts, 0), get(parts, 1))
          case "immune" => (get(parts, 1), get(parts, 0))
        }
      }
    }
    def lineHandler(system:System)(line:String) = {
      val regex = """(\d+) units each with (\d+) hit points (\([^)]+\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r.anchored
      line match {
        case regex(units, hp, prop, attackDamage, attackType, init) =>
          val (weak, immune) = parseProp(prop)
          Group(units.toInt, hp.toInt, attackDamage.toInt, attackType, weak, immune, init.toInt, system)
      }
    }
    def handle(f: List[String] => (String => Boolean) => List[String], system:System) =
      f(input)(!_.startsWith("In")).filter{_.trim.size != 0}.drop(1).map{lineHandler(system)}
    (handle(_.takeWhile, Immune) , handle(_.dropWhile, Infect))
  }

  def fight(immArg:Family, infArg:Family)(boost:Int) = {
    val newinf = infArg.map{_.copy()}
    val newimm = immArg.map{i => i.copy(attackDamage = i.attackDamage + boost)}
    def targetSelect(imm:Family, inf:Family) = {
      (imm ++ inf).sortBy(x=>(-x.effectivePower, -x.init))
      .foldLeft((List[(Group, Group)](), (imm ++ inf).toSet)) { case ((acc, notVisited), attack) =>
        notVisited.filter{attack.isEnemy} match {
          case enemy if enemy.isEmpty => (acc, notVisited)
          case enemy => 
            enemy.maxBy{target => (attack.dealtDamage(target), target.effectivePower, target.init)} match {
              case x if attack.dealtDamage(x) <= 0 => (acc, notVisited)
              case x => (acc :+ (attack -> x), notVisited - x)
            }
        }
      }._1
    }
    def attack(selected:List[(Group, Group)]) = {
      selected.sortBy{-_._1.init}.foldLeft(0){ case (acc, (attack, target)) =>
        val prevUnits = target.units
        target.deal(attack.dealtDamage(target))
        acc + (prevUnits - target.units)
      }
    }
    Iterator.iterate((newimm, newinf, 0)){ case (imm, inf, _) =>
      val numDead = attack(targetSelect(imm, inf))
      (imm.filter{_.isAlive}, inf.filter{_.isAlive}, numDead)
    }.drop(1).find{case (a,b,numDead) => {a.isEmpty || b.isEmpty} || numDead == 0}
     .map{case (a, b, _)=> a ++ b}.get
  }
  def solve(input:Input) = {
    val (immune, infect) = input
    fight(immune, infect)(0).map{_.units}.sum
  }
  def solve2(input:Input) = {
    val (immune, infect) = input
    def isImmuneWin(i:Int) = {
      val res = fight(immune, infect)(i).foldLeft(Set.empty[System]){_ + _.system}
      res.size == 1 && (res.head == Immune)
    }
    Iterator.iterate(1){_ * 2}.take(4000).find{isImmuneWin} match {
      case Some(value) => 
        val res = generalBinarySearch(InEquality(value/2, value)){ i=>
          if(isImmuneWin(i.toInt)) -1 else 1
        }
        fight(immune, infect)(res.toInt).map{_.units}.sum
      case None => println("Not Found... increase num")
    }
  }
}