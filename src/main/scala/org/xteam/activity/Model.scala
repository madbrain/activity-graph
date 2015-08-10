package org.xteam.activity

trait TreeEdge

trait GraphEdge {
  val from : Node
  val to : Node
}

case class DirectedEdge(override val from: Node, override val to: Node) extends GraphEdge
case class UndirectedEdge(override val from: Node, override val to: Node) extends GraphEdge

trait Node
trait BaseNode extends Node

class ClusterNode(elements: Seq[Node]) extends Node
class TopClusterNode(clusterNode: ClusterNode) extends Node
class BottomClusterNode(clusterNode: ClusterNode) extends Node

class InitialNode() extends BaseNode
class FinalNode() extends BaseNode
class ExpansionNode() extends BaseNode // TODO est ce que c'est un ClusterNode ?
class ObjectNode(val name: String) extends BaseNode
class ActivityNode(val name: String) extends BaseNode
class ForkNode() extends BaseNode
class JoinNode() extends BaseNode
class DecisionNode() extends BaseNode
class MergeNode() extends BaseNode
class NoteNode(val content: String) extends BaseNode

case class Graph(nodes: Seq[Node], edges: Seq[GraphEdge]) {
  lazy val outgoings = nodes.map(node => node -> edges.filter(edge => edge.from == node)).toMap
  lazy val incomings = nodes.map(node => node -> edges.filter(edge => edge.to == node)).toMap
  lazy val directedEdges = edges.filter(edge => edge.isInstanceOf[DirectedEdge])
  lazy val undirectedEdges = edges.filter(edge => edge.isInstanceOf[UndirectedEdge])
}

case class Tree(nodes: Seq[Node], edges: Seq[TreeEdge])

case class CompoundGraph(graph: Graph, tree: Tree)

case class Partition(nodes: Seq[Node])

case class Partitions(partitions: Seq[Seq[Partition]]) {
  val rows: Int = partitions.size

}

case class PartitionNode(i: Int) extends Node

case class ActivityGraph(graph: Graph, partitions: Partitions)