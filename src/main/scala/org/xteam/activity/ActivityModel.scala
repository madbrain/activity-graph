package org.xteam.activity


trait BaseNode extends Node

class TopClusterNode(val clusterNode: ClusterNode) extends Node
class BottomClusterNode(val clusterNode: ClusterNode) extends Node

class InitialNode() extends BaseNode
class FinalNode() extends BaseNode
class ExpansionNode() extends BaseNode // TODO est ce que c'est un ClusterNode ?

class ObjectNode(val name: String) extends BaseNode {
  override def toString = s"ActivityNode($name)"
}
object ObjectNode {
  def unapply(node: ObjectNode) = Some(node.name)
}

class ActivityNode(val name: String) extends BaseNode {
  override def toString = s"ActivityNode($name)"
}
object ActivityNode {
  def unapply(node: ActivityNode) = Some(node.name)
}

class ForkNode() extends BaseNode
class JoinNode() extends BaseNode
class DecisionNode() extends BaseNode
class MergeNode() extends BaseNode
class NoteNode(val content: String) extends BaseNode

case class Partition(nodes: Seq[Node])

case class Partitions(partitions: Seq[Seq[Partition]]) {
  val rows: Int = partitions.size
}

case class PartitionNode(i: Int) extends Node

case class ActivityGraph(graph: Graph, partitions: Partitions)