package org.xteam.activity

trait Node

trait Edge {
  val from: Node
  val to: Node
}

class ClusterNode(val elements: Seq[Node]) extends Node

case class DirectedEdge(override val from: Node, override val to: Node) extends Edge
case class UndirectedEdge(override val from: Node, override val to: Node) extends Edge

trait GraphLike {
  val nodes: Seq[Node]
  val edges: Seq[Edge]
  lazy val outgoings = nodes.map(node => node -> edges.filter(edge => edge.from == node)).toMap
  lazy val incomings = nodes.map(node => node -> edges.filter(edge => edge.to == node)).toMap
  lazy val directedEdges = edges.filter(edge => edge.isInstanceOf[DirectedEdge])
  lazy val undirectedEdges = edges.filter(edge => edge.isInstanceOf[UndirectedEdge])
}

case class Tree(nodes: Seq[Node], edges: Seq[Edge])

case class Graph(nodes: Seq[Node], edges: Seq[Edge]) extends GraphLike

case class CompoundGraph(nodes: Seq[Node], edges: Seq[Edge], hierarchyEdges: Seq[Edge]) extends GraphLike

case class LayeredGraph(nodes: Seq[Node], edges: Seq[Edge], layer: Map[Node, Int]) extends GraphLike

case class LayeredCompoundGraph(nodes: Seq[Node], edges: Seq[Edge],
                                hierarchyEdges: Seq[Edge], layer: Map[Node, Int]) extends GraphLike {

  lazy val baseNodes = nodes.filter(node => hierarchyEdges.map(edge => edge.from != node).min)
  lazy val clusterNodes = nodes.filterNot(baseNodes.contains)

  private def computeLeafNodes(node: Node): Seq[Node] = {
      if (baseNodes.contains(node)) {
        Seq(node)
      } else {
        val children = hierarchyEdges.filter(edge => edge.from == node).map(edge => edge.to)
        children.flatMap(node => computeLeafNodes(node))
      }
  }
  
  lazy val leafNodes = nodes.map(node => node -> computeLeafNodes(node)).toMap

  lazy val lmin = nodes.map(node => node -> leafNodes(node).map(node => layer(node)).min).toMap
  lazy val lmax = nodes.map(node => node -> leafNodes(node).map(node => layer(node)).max).toMap

  lazy val hierarchyTree = layer.values.toSet[Int].map(i => {
    val layerNodes = nodes.filter(node => lmin(node) <= i && i <= lmax(node))
    i -> Tree(layerNodes, hierarchyEdges.filter(edge =>
      layerNodes.contains(edge.from) || layerNodes.contains(edge.to)))
  }).toMap
}