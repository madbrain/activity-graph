package org.xteam.activity

object Launcher {
  def main (args: Array[String]) {
    val intialNode = InitialNode()
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

    // preprocess
    // replace each cluster c by two nodes c_top et c_bottom

    // planarization

    // construct nesting graph

    // for each cluster c connect
    //  for each base child v of c in tree hierarchy
    //    add c_top -> v
    //    add v -> c_bottom
    //  for each cluster child d of c in tree hierarchy
    //    add c_top -> d_top
    //    add d_bottom -> c_bottom
  }
}
