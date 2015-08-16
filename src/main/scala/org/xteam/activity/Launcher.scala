package org.xteam.activity

import java.io.{PrintWriter, File}

import org.xteam.activity

object Launcher {

  def main (args: Array[String]): Unit = {

    // layoutActivity(ActivityExamples.buildExample3)

    val graph = ClusteredExamples.example1
    // TODO: should verify that graph clusters are non empty
    // println("==============================")
    graph.hierarchyTree.foreach({ case (i, tree) => println(s"$i => $tree") })
  }
  
  def layoutActivity(inputGraph: ActivityGraph) = {

    val activityGraph = preprocessClusters(inputGraph)

    // => planarization

    // construct nesting graph
    val partitionNodes = 0.to(activityGraph.partitions.rows).map(i => PartitionNode(i))
    val layerEdges = 0.until(activityGraph.partitions.rows).map(i => DirectedEdge(partitionNodes(i), partitionNodes(i+1)))
    val partitionsNodeEdges = activityGraph.partitions.partitions.zipWithIndex.flatMap({ case (row, i) =>
      row.flatMap(partition =>
        partition.nodes.flatMap(node =>
          Seq(DirectedEdge(partitionNodes(i), node), DirectedEdge(node, partitionNodes(i+1))))) })

    // TODO: handle nested cluster
    val clusterNodeEdges = activityGraph.graph.nodes.filter(node => node.isInstanceOf[TopClusterNode]).flatMap(node =>
      node.asInstanceOf[TopClusterNode].clusterNode.elements.map(n => DirectedEdge(node, n))) ++
      activityGraph.graph.nodes.filter(node => node.isInstanceOf[BottomClusterNode]).flatMap(node =>
        node.asInstanceOf[BottomClusterNode].clusterNode.elements.map(n => DirectedEdge(n, node)))
    val nodeEdges = partitionsNodeEdges ++ clusterNodeEdges


    val vpn = partitionNodes ++ activityGraph.graph.nodes
    val epn = layerEdges ++ nodeEdges

    def edgeWeight(edge: Edge) = edge.from match {
      case _ : DecisionNode => 1
      case _ if epn.contains(edge) => 5 * activityGraph.graph.edges.size + 1
      case _ => 5
    }

    val graph = Graph(vpn, epn ++ activityGraph.graph.directedEdges)

    // insert directed edge => Feedback arc problem
    val feedbackArc = new FeedbackArcSolver().solve(graph, edgeWeight)

    if (feedbackArc.nonEmpty) {
      throw new RuntimeException("separate edges: " + feedbackArc)
    }

    // insert undirected edge
    if (activityGraph.graph.undirectedEdges.nonEmpty) {
      throw new RuntimeException("insert undirected: " + activityGraph.graph.undirectedEdges)
    }

    // should be epn + activityGraph.graph.directedEdges
    //    - feedbackArc + activityGraph.graph.undirectedEdges + newFeedback
    val newEdges = graph.edges

    val layeredGraph = doLayerAssignment(Graph(vpn, newEdges), epn)

    val maxRows = layeredGraph.layer.values.max
    val nodePerRanks = 0.to(maxRows).map(i => layeredGraph.nodes.filter(node => layeredGraph.layer(node) == i))
    nodePerRanks.foreach(rank => println(rank))

    outputToDot("out.dot", layeredGraph)

    // insert dummy border node => partition and cluster

    // crossing reduction

    // horizontal coordinate assignment (optionel mais interessant pour le debug)

    // creating planar embedding

    // rerouting (optional?)

    // => Orthogonalisation

    // make edge shapes

    // ...
  }

  def preprocessClusters(activityGraph: ActivityGraph): ActivityGraph = {
    val clusterNodes = activityGraph.graph.nodes.filter(node => node.isInstanceOf[ClusterNode])
    if (clusterNodes.nonEmpty) {
      case class TopBottomAssociation(clusterNode: Node, topNode: TopClusterNode, bottomNode: BottomClusterNode,
                                      incomings: Seq[Edge], outgoings: Seq[Edge])
      val topBottomTuples = clusterNodes.asInstanceOf[Seq[ClusterNode]].map(clusterNode => {
        TopBottomAssociation(clusterNode,
          new TopClusterNode(clusterNode), new BottomClusterNode(clusterNode),
          activityGraph.graph.incomings(clusterNode),
          activityGraph.graph.outgoings(clusterNode)
        )
      })
      val newNodes = topBottomTuples.flatMap(asso => Seq(asso.topNode, asso.bottomNode))
      val clusterEdges = topBottomTuples.flatMap(asso => asso.incomings ++ asso.outgoings)
      val newEdges = topBottomTuples.flatMap(asso =>
        asso.incomings.map(edge => new DirectedEdge(edge.from, asso.topNode)) ++
          asso.outgoings.map(edge => new DirectedEdge(asso.bottomNode, edge.to))
      )
      val newGraph = Graph(activityGraph.graph.nodes.filterNot(clusterNodes.contains) ++ newNodes,
        activityGraph.graph.edges.filterNot(clusterEdges.contains) ++ newEdges)
      val topBottomByNode = topBottomTuples.map(asso => asso.clusterNode -> Seq(asso.topNode, asso.bottomNode)).toMap
      val newPartitions = Partitions(activityGraph.partitions.partitions.map(row =>
        row.map(partition => Partition(partition.nodes.flatMap(node =>
          if(clusterNodes.contains(node)) topBottomByNode(node) else Seq(node))))))
      ActivityGraph(newGraph, newPartitions)
    } else {
      activityGraph
    }
  }

  class DummyNode extends Node

  def doLayerAssignment(graph: Graph, epn: Seq[Edge]) = {
    val order = new TopologicalSorter().sort(graph)

    // remove nesting graph edges
    val realEdges = graph.edges.filterNot(epn.contains)

    // insert dummy nodes for long edges
    val longEdges = realEdges.filter(edge => (order(edge.to) - order(edge.from)) > 1)

    val tuples = longEdges.map(edge => {
      val dummyNodesToLayer = (order(edge.from)+1).to(order(edge.to)-1).map(l => new DummyNode() -> l)
      val dummyNodes = dummyNodesToLayer.map({ case (node, l) => node })
      val newEdges = (edge.from +: dummyNodes).zip(dummyNodes :+ edge.to)
        .map({ case (from, node) => DirectedEdge(from, node) })
      (dummyNodesToLayer, newEdges)
    })
    val newNodesToLayer = tuples.flatMap({ case (nodeToLayer, edges) => nodeToLayer }).toMap
    val newEdges = tuples.flatMap({ case (nodeToLayer, edges) => edges })

    LayeredGraph(graph.nodes ++ newNodesToLayer.keys,
      realEdges.filterNot(longEdges.contains) ++ newEdges,
      order ++ newNodesToLayer)
  }

  def outputToDot(fileName: String, graph: GraphLike): Unit = {
    val nodeIndex = graph.nodes.zip(1.to(graph.nodes.size)).toMap
    val writer = new PrintWriter(new File(fileName))
    writer.println("digraph G {")
    graph.nodes.foreach(node => {
      val color = node match {
        case PartitionNode(_) => "blue"
        case x : TopClusterNode => "red"
        case x : BottomClusterNode => "red"
        case x : ForkNode => "pink"
        case x : JoinNode => "pink"
        case x : DecisionNode => "green"
        case x : MergeNode => "green"
        case x : DummyNode => "grey"
        case _ => "black"
      }
      val label = node match {
        case ActivityNode(name) => name
        case ObjectNode(name) => name
        case _ => s"N${nodeIndex(node)}"
      }
      writer.println("n_" + nodeIndex(node) + s"""[shape=box,color=\"$color\",label=\"$label\"];""")
    })
    graph.nodes.foreach(node => {
      graph.outgoings(node).foreach(edge => {
        writer.println("n_" + nodeIndex(node) + " -> n_" + nodeIndex(edge.to) + ";")
      })
    })
    writer.println("}")
    writer.close()
  }


}
