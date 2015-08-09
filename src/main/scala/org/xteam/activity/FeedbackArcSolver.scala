package org.xteam.activity

import scala.collection.mutable

class FeedbackArcSolver {

  def solve(graph: Graph, edgeWeight: (GraphEdge) => Int): Seq[GraphEdge] = {
    val F = new mutable.ArrayBuffer[GraphEdge]
    val weights: mutable.Map[GraphEdge, Int] = mutable.Map(graph.edges.map(edge => edge -> edgeWeight(edge)): _*)
    Iterator.continually({
      new CycleFinder().findCycle(Graph(graph.nodes, graph.edges.filterNot(F.contains))) match {
        case Some(cycle) =>
          val minWeight = cycle.map(edge => weights(edge)).min
          cycle.foreach(edge => weights(edge) -= minWeight)
          F ++= weights.filter({ case (edge, weight) => weight == 0 }).keys
          true
        case None => false
      }
    }).takeWhile(identity)

    F.clone().foreach(edge => {
      new CycleFinder().findCycle(Graph(graph.nodes, graph.edges.filterNot(F.contains) :+ edge)) match {
        case None =>
          F -= edge
        case Some(_) =>
      }
    })
    return F
  }

}