package org.xteam.activity

import scala.collection.mutable

object CycleFinder {

  sealed class Color
  object Color {
    val White = new Color
    val Grey = new Color
    val Black = new Color
  }

  def findCycle(graph: Graph): Option[Seq[Edge]] = {
    val color = mutable.Map(graph.nodes.map(node => node -> Color.White): _*)
    graph.nodes.foreach(node => {
      if (color(node) == Color.White) {
        visit(node, graph, color, Seq()) match {
          case Some(c) => return Some(c)
          case _ =>
        }
      }
    })
    None
  }

  private def visit(node: Node, graph: Graph,
            color: mutable.Map[Node, Color], stack: Seq[Edge]): Option[Seq[Edge]] = {
    color(node) = Color.Grey
    graph.outgoings(node).foreach(edge => {
      val to = edge.to
      val newStack = stack :+ edge
      if (color(to) == Color.Grey) {
        return Some(newStack.dropWhile(e => e.from != edge.to))
      }
      if (color(to) == Color.White) {
        visit(to, graph, color, newStack) match {
          case Some(c) => return Some(c)
          case _ =>
        }
      }
    })
    color(node) = Color.Black
    None
  }
}
