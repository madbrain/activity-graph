package org.xteam.activity

trait TreeEdge

trait GraphEdge

trait DirectedEdge extends GraphEdge
trait UndirectedEdge extends GraphEdge

trait Node {
  var row : Int = 0
  var column : Int = 0
}
trait BaseNode extends Node

case class ClusterNode()

case class InitialNode() extends BaseNode
case class FinalNode() extends BaseNode
case class ExpansionNode() extends BaseNode // TODO est ce que c'est une ClusterNode ?
case class ObjectNode(name: String) extends BaseNode
case class ActivityNode(name: String) extends BaseNode
case class ForkJoinNode() extends BaseNode
case class DecisionMergeNode() extends BaseNode
case class NoteNode() extends BaseNode

case class Graph(nodes: Seq[Node], edges: Seq[GraphEdge])

case class Tree(nodes: Seq[Node], edges: Seq[TreeEdge])

case class CompoundGraph(graph: Graph, tree: Tree)