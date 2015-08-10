package org.xteam.activity

import org.xteam.activity

object Launcher {
  def main (args: Array[String]) {

    val activityGraph = buildExample2

    // => preprocess

    // replace each cluster c by two nodes c_top et c_bottom
    val clusterNodes = activityGraph.graph.nodes.filter(node => node.isInstanceOf[ClusterNode])
    val bottomEdges = activityGraph.graph.outgoings(clusterNode).map(edge => new DirectedEdge(topNode, edge.to))
    if (clusterNodes.nonEmpty) {
      clusterNodes.asInstanceOf[Seq[ClusterNode]].foreach(clusterNode => {
        val topNode = new TopClusterNode(clusterNode)
        val bottomNode = new BottomClusterNode(clusterNode)
        val topEdges = activityGraph.graph.incomings(clusterNode).map(edge => new DirectedEdge(edge.from, topNode))
        throw new RuntimeException("accumulate nodes")
      })
      throw new RuntimeException("replace cluster nodes")
      // activityGraph.graph.nodes = activityGraph.graph.nodes - clusterNodes + topNodes + bottomNodes
      // activityGraph.graph.edges = activityGraph.graph.edges - clusterEdges + topEdges + bottomEdges
    }

    // => planarization

    // construct nesting graph

    // for each cluster c connect
    //  for each base child v of c in tree hierarchy
    //    add c_top -> v
    //    add v -> c_bottom
    //  for each cluster child d of c in tree hierarchy
    //    add c_top -> d_top
    //    add d_bottom -> c_bottom

    val partitionNodes = 0.to(activityGraph.partitions.rows).map(i => PartitionNode(i))
    val layerEdges = 0.until(activityGraph.partitions.rows).map(i => DirectedEdge(partitionNodes(i), partitionNodes(i+1)))
    val nodeEdges = activityGraph.partitions.partitions.zipWithIndex.flatMap({ case (row, i) =>
      row.flatMap(partition =>
        partition.nodes.flatMap(node =>
          Seq(DirectedEdge(partitionNodes(i), node), DirectedEdge(node, partitionNodes(i+1))))) })

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
    println(order)

    // remove nesting graph edges

    // insert dummy border node => partition and cluster

    // crossing reduction

    // horizontal coordinate assignment (optionel mais interessant pour le debug)

    // creating planar embedding

    // rerouting (optional?)

    // => Orthogonalisation

    // make edge shapes

    // ...
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
