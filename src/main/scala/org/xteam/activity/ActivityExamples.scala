package org.xteam.activity

object ActivityExamples {

  def buildExample1: ActivityGraph = {
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
    val undirectedEdges = List[Edge]()
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
    val undirectedEdges = List[Edge]()
    val edges = directedEdges ++ undirectedEdges

    val partitions = Partitions(List(
      List(
        Partition(nodes))))

    ActivityGraph(Graph(nodes, edges), partitions)
  }

  def buildExample3: ActivityGraph = {
    val initialNode = new InitialNode()
    val requestDepositSlip = new ActivityNode("Request Deposit Slip")
    val createDepositSlip = new ActivityNode("Create Deposit Slip")
    val filloutDepositSlip = new ActivityNode("Fillout and Submit Deposit Slip")
    val fork = new ForkNode()
    val verifyIdentity = new ActivityNode("Verify Identity")
    val rejectedOrAccepted = new DecisionNode()
    val markDepositAsunapproved = new ActivityNode("Mark Deposit as Unapproved")
    val approveDeposit = new ActivityNode("Approve Deposit")
    val merge = new MergeNode()
    val join = new JoinNode()
    val updatePassbook = new ActivityNode("Update Passbook")
    val examinePassbook = new ActivityNode("Examine Passbook")
    val finalNode = new FinalNode()

    val clusterNode = new ClusterNode(Seq(verifyIdentity, markDepositAsunapproved))

    val nodes = List(initialNode, requestDepositSlip, createDepositSlip, rejectedOrAccepted,
      filloutDepositSlip, fork, verifyIdentity, rejectedOrAccepted, markDepositAsunapproved,
      approveDeposit, join, merge, updatePassbook, examinePassbook, finalNode, clusterNode)

    val directedEdges = List(
      DirectedEdge(initialNode, requestDepositSlip),
      DirectedEdge(requestDepositSlip, createDepositSlip),
      DirectedEdge(createDepositSlip, filloutDepositSlip),
      DirectedEdge(filloutDepositSlip, fork),
      DirectedEdge(fork, rejectedOrAccepted),
      DirectedEdge(rejectedOrAccepted, markDepositAsunapproved),
      DirectedEdge(rejectedOrAccepted, approveDeposit),
      DirectedEdge(markDepositAsunapproved, merge),
      DirectedEdge(approveDeposit, merge),
      DirectedEdge(merge, join),
      DirectedEdge(fork, verifyIdentity),
      DirectedEdge(verifyIdentity, join),
      DirectedEdge(merge, join),
      DirectedEdge(join, updatePassbook),
      DirectedEdge(updatePassbook, examinePassbook),
      DirectedEdge(examinePassbook, finalNode)
    )
    val undirectedEdges = List[Edge]()
    val edges = directedEdges ++ undirectedEdges

    val partitions = Partitions(List(
      List(
        Partition(List(initialNode, requestDepositSlip, filloutDepositSlip, examinePassbook)),
        Partition(List(createDepositSlip, fork, join, verifyIdentity, markDepositAsunapproved, updatePassbook)),
        Partition(List(rejectedOrAccepted, approveDeposit, merge))
      )))

    ActivityGraph(Graph(nodes, edges), partitions)
  }

}
