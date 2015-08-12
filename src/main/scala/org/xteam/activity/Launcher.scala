package org.xteam.activity

import java.io.{PrintWriter, File}

import org.xteam.activity

object Launcher {
  def main (args: Array[String]) {

    val activityGraph = preprocessClusters(buildExample2)

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

    def edgeWeight(edge: GraphEdge) = edge.from match {
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

    // do layering => topological sort
    val order = new TopologicalSorter().sort(Graph(vpn, newEdges))
    val maxRows = order.values.max
    val nodePerRanks = 0.to(maxRows).map(i => order.filter({ case (node, rank) => rank == i}).keys.toList)
    nodePerRanks.foreach(rank => println(rank))

    outputToDot("out.dot", Graph(vpn, newEdges))

    // remove nesting graph edges

    // insert dummy nodes for long edges

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
                                      incomings: Seq[GraphEdge], outgoings: Seq[GraphEdge])
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

  def outputToDot(fileName: String, graph: Graph): Unit = {
    val nodeIndex = graph.nodes.zip(1.to(graph.nodes.size)).toMap
    val writer = new PrintWriter(new File(fileName))
    writer.println("digraph G {")
    graph.nodes.foreach(node => {
      val color = node match {
        case PartitionNode(_) => "blue"
        case x : TopClusterNode => "red"
        case x : BottomClusterNode => "red"
        case _ => "black"
      }
      writer.println("n_" + nodeIndex(node) + s"""[shape=box,color=\"${color}\"];""")
    })
    graph.nodes.foreach(node => {
      graph.outgoings(node).foreach(edge => {
        writer.println("n_" + nodeIndex(node) + " -> n_" + nodeIndex(edge.to) + ";")
      })
    })
    writer.println("}")
    writer.close()
  }

  def buildExample: ActivityGraph = {
    val initialNode = new InitialNode()
    val receiveOrder = new ActivityNode("Receive Order")
    val rejectedOrAccepted = new DecisionNode()
    val fillOrder = new ActivityNode("Fill Order")
    val fork = new ForkNode()
    val sendInvoice = new ActivityNode("Send Invoice")
    val invoice = new ObjectNode("Invoice")
    val makePayment = new ActivityNode("Make Payment")
    val shipOrder = new ActivityNode("shipOrder")
    val join = new JoinNode()
    val merge = new MergeNode()
    val closeOrder = new ActivityNode("Close Order")
    val finalNode = new FinalNode()

    val nodes = List(initialNode, receiveOrder, rejectedOrAccepted, fillOrder,
      fork, sendInvoice, invoice, makePayment, shipOrder, join, merge, closeOrder, finalNode)

    val directedEdges = List(
      DirectedEdge(initialNode, receiveOrder),
      DirectedEdge(receiveOrder, rejectedOrAccepted),
      DirectedEdge(rejectedOrAccepted, fillOrder),
      DirectedEdge(rejectedOrAccepted, merge),
      DirectedEdge(fillOrder, fork),
      DirectedEdge(fork, sendInvoice),
      DirectedEdge(fork, shipOrder),
      DirectedEdge(sendInvoice, invoice),
      DirectedEdge(invoice, makePayment),
      DirectedEdge(shipOrder, join),
      DirectedEdge(makePayment, join),
      DirectedEdge(join, merge),
      DirectedEdge(merge, closeOrder),
      DirectedEdge(closeOrder, finalNode)
    )
    val undirectedEdges = List[GraphEdge]()
    val edges = directedEdges ++ undirectedEdges

    val partitions = Partitions(List(
      List(
        Partition(List(initialNode, receiveOrder, rejectedOrAccepted, fillOrder, fork)),
        Partition(List(sendInvoice, invoice))),
      List(
        Partition(List(shipOrder, join, merge, closeOrder, finalNode)),
        Partition(List(makePayment)))))

    ActivityGraph(Graph(nodes, edges), partitions)
  }

  def buildExample2: ActivityGraph = {
    val initialNode = new InitialNode()
    val receiveOrder = new ActivityNode("Receive Order")
    val rejectedOrAccepted = new DecisionNode()
    val fillOrder = new ActivityNode("Fill Order")
    val fork = new ForkNode()
    val sendInvoice = new ActivityNode("Send Invoice")
    val invoice = new ObjectNode("Invoice")
    val makePayment = new ActivityNode("Make Payment")
    val shipOrder = new ActivityNode("shipOrder")
    val join = new JoinNode()
    val merge = new MergeNode()
    val closeOrder = new ActivityNode("Close Order")
    val finalNode = new FinalNode()
    val orderCancelRequest = new ActivityNode("Order Cancel Request")
    val cancelRequest = new ActivityNode("Cancel Request")
    val clusterNode = new ClusterNode(Seq(receiveOrder, rejectedOrAccepted, fillOrder, fork, shipOrder, orderCancelRequest))

    val nodes = List(initialNode, receiveOrder, rejectedOrAccepted, fillOrder,
      fork, sendInvoice, invoice, makePayment, shipOrder, join, merge, closeOrder, finalNode, orderCancelRequest,
      cancelRequest, clusterNode)

    val directedEdges = List(
      DirectedEdge(initialNode, receiveOrder),
      DirectedEdge(receiveOrder, rejectedOrAccepted),
      DirectedEdge(rejectedOrAccepted, fillOrder),
      DirectedEdge(rejectedOrAccepted, merge),
      DirectedEdge(fillOrder, fork),
      DirectedEdge(fork, sendInvoice),
      DirectedEdge(fork, shipOrder),
      DirectedEdge(sendInvoice, invoice),
      DirectedEdge(invoice, makePayment),
      DirectedEdge(shipOrder, join),
      DirectedEdge(makePayment, join),
      DirectedEdge(join, merge),
      DirectedEdge(merge, closeOrder),
      DirectedEdge(closeOrder, finalNode),
      DirectedEdge(orderCancelRequest, cancelRequest),
      DirectedEdge(cancelRequest, finalNode)
    )
    val undirectedEdges = List[GraphEdge]()
    val edges = directedEdges ++ undirectedEdges

    val partitions = Partitions(List(
      List(
        Partition(nodes))))

    ActivityGraph(Graph(nodes, edges), partitions)
  }
}
