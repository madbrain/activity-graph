package org.xteam.activity


object ClusteredExamples {

  class ExampleNode(val i: Int) extends Node {
    override def toString = s"ExampleNode($i)"
  }

  class ExampleCluster(val i: Int) extends Node {
    override def toString = s"ExampleCluster($i)"
  }

  def example1: LayeredCompoundGraph = {

    // 9, 11, 14 are dummy nodes
    val baseNodes = 0.to(18).map(i => new ExampleNode(i))

    val clusterNodes = 0.to(5).map(i => new ExampleCluster(i))

    val edges = Seq(
      DirectedEdge(baseNodes(1), baseNodes(6)),
      DirectedEdge(baseNodes(1), clusterNodes(4)),
      DirectedEdge(clusterNodes(0), clusterNodes(2)),
      DirectedEdge(baseNodes(2), clusterNodes(3)),
      DirectedEdge(baseNodes(2), clusterNodes(4)),
      DirectedEdge(baseNodes(3), baseNodes(8)),
      DirectedEdge(baseNodes(3), baseNodes(9)),
      DirectedEdge(baseNodes(4), baseNodes(8)),
      DirectedEdge(baseNodes(4), baseNodes(10)),
      DirectedEdge(baseNodes(5), clusterNodes(5)),
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

    val hierarchyEdges = Seq(
      DirectedEdge(clusterNodes(0), baseNodes(0)),
      DirectedEdge(clusterNodes(0), baseNodes(1)),
      DirectedEdge(clusterNodes(1), baseNodes(2)),
      DirectedEdge(clusterNodes(1), baseNodes(3)),
      DirectedEdge(clusterNodes(2), baseNodes(6)),
      DirectedEdge(clusterNodes(2), baseNodes(12)),
      DirectedEdge(clusterNodes(3), clusterNodes(4)),
      DirectedEdge(clusterNodes(3), baseNodes(8)),
      DirectedEdge(clusterNodes(3), baseNodes(14)),
      DirectedEdge(clusterNodes(3), baseNodes(18)),
      DirectedEdge(clusterNodes(4), baseNodes(7)),
      DirectedEdge(clusterNodes(4), baseNodes(13)),
      DirectedEdge(clusterNodes(5), baseNodes(10)),
      DirectedEdge(clusterNodes(5), baseNodes(11)),
      DirectedEdge(clusterNodes(5), baseNodes(16)),
      DirectedEdge(clusterNodes(5), baseNodes(17))
    )

    val order = Map[Node, Int](
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

    LayeredCompoundGraph(baseNodes ++ clusterNodes, edges, hierarchyEdges, order)
  }
}
