package org.xteam.activity

import scala.collection.mutable

class TopologicalSorter() {

  def sort(graph: Graph): Map[Node, Int] = {
    val layer = mutable.Map[Node, Int]()
    val inDegree = mutable.Map[Node, Int](graph.nodes.map(node => node -> graph.incomings(node).size): _*)
    val sources = graph.nodes.filter(node => inDegree(node) == 0)
    val stack = mutable.Stack[Node]()
    sources.foreach(node => {
      layer(node) = 0
      stack.push(node)
    })
    while (stack.nonEmpty) {
      val node = stack.pop()
      if (! layer.contains(node)) {
        layer(node) = graph.incomings(node).map(edge => layer(edge.from)+1).max
      }
      graph.outgoings(node).foreach(edge => {
        inDegree(edge.to) -= 1
        if (inDegree(edge.to) == 0) {
          stack.push(edge.to)
        }
      })
    }
    return layer.toMap
  }

}
