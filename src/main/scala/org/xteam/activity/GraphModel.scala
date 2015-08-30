package org.xteam.activity

trait Node

trait Edge {
  val from: Node
  val to: Node

  def opposite(node: Node): Node = if (node == from) to else from
}

class ClusterNode(val elements: Seq[Node]) extends Node

case class DirectedEdge(override val from: Node, override val to: Node) extends Edge
case class UndirectedEdge(override val from: Node, override val to: Node) extends Edge
case class WeightedEdge(override val from: Node, override val to: Node, weight: Int) extends Edge

trait GraphLike {
  val nodes: Seq[Node]
  val edges: Seq[Edge]
  lazy val outgoings = nodes.map(node => node -> edges.filter(edge => edge.from == node)).toMap
  lazy val incomings = nodes.map(node => node -> edges.filter(edge => edge.to == node)).toMap
  lazy val directedEdges = edges.filter(edge => edge.isInstanceOf[DirectedEdge])
  lazy val undirectedEdges = edges.filter(edge => edge.isInstanceOf[UndirectedEdge])
}

trait TreeNode extends Node {

  def children: Seq[Node]

  def copyWith(children: Seq[Node]): TreeNode

  def leaves: Seq[Node] = children.flatMap({
    case node: TreeNode => node.leaves
    case node => Seq(node)
  })

  lazy val withChildren: Seq[Node] = this +: children.flatMap({
    case node: TreeNode => node.withChildren
    case node => Seq(node)
  })

  def filter(f: Node => Boolean): TreeNode = {
    copyWith(children.filter(node => f(node)).map({
      case node: TreeNode => node.filter(f)
      case node => node
    }))
  }
}

case class Graph(nodes: Seq[Node], edges: Seq[Edge]) extends GraphLike

case class LayeredGraph(nodes: Seq[Node], edges: Seq[Edge],
                                layer: Map[Node, Int]) extends GraphLike

case class CompoundLayeredGraph(nodes: Seq[Node], edges: Seq[Edge], root: TreeNode,
                                layer: Map[Node, Int]) extends GraphLike {

  lazy val baseNodes = nodes.filter({ case x: TreeNode => false case _ => true})
  lazy val clusterNodes = nodes.filterNot(baseNodes.contains)

  lazy val lmin = nodes.map({
    case node: TreeNode => node -> node.leaves.map(n => layer(n)).min
    case node => node -> layer(node)
  }).toMap
  lazy val lmax = nodes.map({
    case node: TreeNode => node -> node.leaves.map(n => layer(n)).max
    case node => node -> layer(node)
  }).toMap

  lazy val hierarchyTree = layer.values.toSet[Int].map(i => {
    val layerNodes = nodes.filter(node => lmin(node) <= i && i <= lmax(node))
    i -> root.filter(n => layerNodes.contains(n))
  }).toMap
}
