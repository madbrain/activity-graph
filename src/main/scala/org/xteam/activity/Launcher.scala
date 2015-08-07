package org.xteam.activity

object Launcher {
  def main (args: Array[String]) {
    val initialNode = InitialNode()
    val receiveOrder = ActivityNode("Receive Order")
    val rejectedOrAccepted = DecisionMergeNode()
    val fillOrder = ActivityNode("Fill Order")
    val fork = ForkJoinNode()
    val sendInvoice = ActivityNode("Send Invoice")
    val invoice = ObjectNode("Invoice")
    val makePayment = ActivityNode("Make Payment")
    val shipOrder = ActivityNode("shipOrder")
    val join = ForkJoinNode()
    val merge = DecisionMergeNode()
    val closeOrder = ActivityNode("Close Order")
    val finalNode = FinalNode()

    // TODO define edges
    val edges = List(
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

    def px(node: Node): Int = {
      if (node == initialNode) 0
      else if (node == receiveOrder) 0
      else if (node == rejectedOrAccepted) 0
      else if (node == fillOrder) 0
      else if (node == fork) 0
      else if (node == shipOrder) 0
      else if (node == join) 0
      else if (node == merge) 0
      else if (node == closeOrder) 0
      else if (node == finalNode) 0
      else if (node == sendInvoice) 1
      else if (node == invoice) 1
      else if (node == makePayment) 1
      else throw new RuntimeException("unknown node")
    }

    def py(node: Node): Int = {
      if (node == initialNode) 0
      else if (node == receiveOrder) 0
      else if (node == rejectedOrAccepted) 0
      else if (node == fillOrder) 0
      else if (node == fork) 0
      else if (node == sendInvoice) 0
      else if (node == invoice) 0
      else if (node == shipOrder) 1
      else if (node == join) 1
      else if (node == merge) 1
      else if (node == closeOrder) 1
      else if (node == finalNode) 1
      else if (node == makePayment) 1
      else throw new RuntimeException("unknown node")
    }

    // => preprocess
    // replace each cluster c by two nodes c_top et c_bottom

    // => planarization

    // construct nesting graph

    // for each cluster c connect
    //  for each base child v of c in tree hierarchy
    //    add c_top -> v
    //    add v -> c_bottom
    //  for each cluster child d of c in tree hierarchy
    //    add c_top -> d_top
    //    add d_bottom -> c_bottom

    // for each partition p
    //   create p_y_i et p_y_i+1
    //   add p_y_i -> p_y_i+1
    //   for each element e in partition p
    //     add p_y_i -> e
    //     add e -> p_y_i+1

    // insert directed edge => Feedback arc problem
    // insert undirected edge

    // do layering => tri topologique

    // insert dummy border node => partition and cluster

    // crossing reduction

    // horizontal coordinate assignment (optionel mais interessant pour le debug)

    // creating planar embedding

    // rerouting

    // => Orthogonalisation

    // make edge shapes

    // ...
  }
}
