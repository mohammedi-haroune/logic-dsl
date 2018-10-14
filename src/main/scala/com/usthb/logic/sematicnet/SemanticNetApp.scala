package com.usthb.logic.sematicnet

import java.nio.file.{Files, Paths}

import scala.collection.JavaConverters._
import scala.collection.mutable.Set
import scala.sys.process.{Process, _}

trait SemanticNetApp {
  implicit var net: SemanticNet = SemanticNet.empty

  def initialise: SemanticNet = SemanticNet.empty
  def showNet(): Unit = println(net)
  def showNodes(): Unit = println(net.nodes.mkString("nodes [", ", ", "]"))
  def showRelations(): Unit =
    println(net.relations.mkString("relations [\n", "\n", "\n]"))
  def showMarkers(): Unit = println(net.markers)

  def properties(node: Node): String =
    node.relations
      .filter(_.isInstanceOf[BinaryRelation])
      .map(_.asInstanceOf[BinaryRelation])
      .filter(_.arg1 == node)
      .map(r => s"${r.name.name} = ${r.arg2}")
      .mkString(s"properties of $node [", ", ", "]")

  def solve(question: Question): String = {
    Marker.initialize()
    val s = question.solve
    if (s.isEmpty) ""
    else
      s.map { case (n1, n2) => s"$n1 ${question.relation.name} $n2" }
        .mkString("solutions [", ",", "]")
  }

  def solveOne(question: Question): String = {
    Marker.initialize()
    question.solve.headOption
      .map { case (n1, n2) => s"solution: $n1 ${question.relation.name} $n2" }
      .getOrElse("no solution found")
  }

  def save(file: String): Process = {
    Files.write(Paths.get(s"$file.dot"), Seq(net.graph).asJava)
    Seq("dot", "-Tpng", s"$file.dot", s"-o$file.png").run
  }

  def save: Process = save("net")

  def saturer() = {
    var newsize = net.relations.size
    var oldsize = newsize
    val closed = Set.empty[(BinaryRelation, BinaryRelation)]

    def changed = newsize != oldsize

    do {
      println(s"oldsize = ${oldsize}")
      println(s"newsize = ${newsize}")

      net.relations.toArray
        .filter(_.isTransitive)
        .filter(_.isInstanceOf[BinaryRelation])
        .map(_.asInstanceOf[BinaryRelation])
        .combinations(2)
        .map(a => (a(0), a(1)))
        .filterNot(closed.contains)
        .foreach {
          case (r1, r2) if r1.name == r2.name =>
            r1.name match {
              case 'Is if r1.arg2 == r2.arg1 =>
                closed += ((r1, r2))
                net.addRelation(Is(r1.arg1, r2.arg2))
              case 'Is if r2.arg2 == r1.arg1 =>
                closed += ((r2, r1))
                net.addRelation(Is(r2.arg1, r1.arg2))
              case r if r1.arg2 == r2.arg1 =>
                closed += ((r1, r2))
                net.addRelation(new BinaryRelation(r, r1.arg1, r2.arg2))
              case r if r2.arg2 == r1.arg1 =>
                closed += ((r2, r1))
                net.addRelation(new BinaryRelation(r, r2.arg1, r1.arg2))
              case _ => println(s"not supported yet: ($r1, $r2)")
            }
          case (r1, r2) if r1.name == 'instance && r2.name == 'is_a =>
            if (r1.arg2 == r2.arg1) {
              closed += ((r1, r2))
              net.addRelation(Instance(r1.arg1, r2.arg2))
            } else if (r2.arg2 == r1.arg1) {
              closed += ((r2, r1))
              net.addRelation(Is(r2.arg1, r1.arg2))
            }
          case (r2, r1) if r1.name == 'instance && r2.name == 'is_a =>
            if (r1.arg2 == r2.arg1) {
              closed += ((r1, r2))
              net.addRelation(Instance(r1.arg1, r2.arg2))
            } else if (r2.arg2 == r1.arg1) {
              closed += ((r2, r1))
              net.addRelation(Is(r2.arg1, r1.arg2))
            }
        }
      oldsize = newsize
      newsize = net.relations.size
    } while (changed)
  }
}
