package org.xteam.activity


object ClusteredExamples {

  class ExampleNode(val i: Int) extends Node {
    override def toString = s"ExampleNode($i)"
  }

  class ExampleCluster(val i: Int, val children: Seq[Node]) extends Node with TreeNode {
    override def copyWith(children: Seq[Node]): TreeNode = new ExampleCluster(i, children)
    override def equals(obj: Any) = obj match {
      case x: ExampleCluster => i == x.i
      case _ => false
    }
    override def hashCode() = i
    override def toString = s"ExampleCluster($i)"
  }

  def example1: CompoundLayeredGraph = {

    // 9, 11, 14 are dummy nodes
    val baseNodes = 0.to(18).map(i => new ExampleNode(i))

    val cluster6 = new ExampleCluster(6, Seq(baseNodes(10), baseNodes(11), baseNodes(16), baseNodes(17)))
    val cluster5 = new ExampleCluster(5, Seq(baseNodes(7), baseNodes(13)))
    val cluster4 = new ExampleCluster(4, Seq(cluster5, baseNodes(8), baseNodes(14), baseNodes(18)))
    val cluster3 = new ExampleCluster(3, Seq(baseNodes(6), baseNodes(12)))
    val cluster2 = new ExampleCluster(2, Seq(baseNodes(2), baseNodes(3)))
    val cluster1 = new ExampleCluster(1, Seq(baseNodes(0), baseNodes(1)))
    val cluster0 = new ExampleCluster(0, Seq(cluster1, cluster2, cluster3, cluster4, cluster6,
      baseNodes(4), baseNodes(5), baseNodes(9), baseNodes(15)))

    val edges = Seq(
      DirectedEdge(baseNodes(1), baseNodes(6)),
      DirectedEdge(baseNodes(1), cluster5),
      DirectedEdge(cluster1, cluster3),
      DirectedEdge(baseNodes(2), cluster4),
      DirectedEdge(baseNodes(2), cluster5),
      DirectedEdge(baseNodes(3), baseNodes(8)),
      DirectedEdge(baseNodes(3), baseNodes(9)),
      DirectedEdge(baseNodes(4), baseNodes(8)),
      DirectedEdge(baseNodes(4), baseNodes(10)),
      DirectedEdge(baseNodes(5), cluster6),
      DirectedEdge(baseNodes(5), baseNodes(11)),
      DirectedEdge(baseNodes(7), baseNodes(12)),
      DirectedEdge(baseNodes(7), baseNodes(13)),
      DirectedEdge(baseNodes(7), baseNodes(15)),
      DirectedEdge(baseNodes(8), baseNodes(14)),
      DirectedEdge(baseNodes(9), baseNodes(15)),
      DirectedEdge(baseNodes(10), baseNodes(15)),
      DirectedEdge(baseNodes(10), baseNodes(16)),
      DirectedEdge(baseNodes(10), baseNodes(17)),
      DirectedEdge(baseNodes(11), baseNodes(16)),
      DirectedEdge(baseNodes(13), baseNodes(18)),
      DirectedEdge(baseNodes(14), baseNodes(18)),
      DirectedEdge(baseNodes(15), baseNodes(18))
    )

    val layers = Map[Node, Int](
      baseNodes(0)  -> 0,
      baseNodes(1)  -> 0,
      baseNodes(2)  -> 0,
      baseNodes(3)  -> 0,
      baseNodes(4)  -> 0,
      baseNodes(5)  -> 0,
      baseNodes(6)  -> 1,
      baseNodes(7)  -> 1,
      baseNodes(8)  -> 1,
      baseNodes(9)  -> 1,
      baseNodes(10) -> 1,
      baseNodes(11) -> 1,
      baseNodes(12) -> 2,
      baseNodes(13) -> 2,
      baseNodes(14) -> 2,
      baseNodes(15) -> 2,
      baseNodes(16) -> 2,
      baseNodes(17) -> 2,
      baseNodes(18) -> 3
    )

    val clusterNodes = Seq(
      cluster0,
      cluster1,
      cluster2,
      cluster3,
      cluster4,
      cluster5,
      cluster6
    )

    CompoundLayeredGraph(baseNodes ++ clusterNodes, edges, cluster0, layers)
  }
}
