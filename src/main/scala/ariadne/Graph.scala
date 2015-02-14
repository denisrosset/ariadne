package com.faacets.ariadne


import scala.collection.parallel.ParSeq

import spire.syntax.cfor._
import spire.util.Opt

trait Graph {
  def numVertices: Int
  def numEdges: Int
  def endVertices(e: EIndex): Set[VIndex]
  def edges(v: VIndex): Set[EIndex]
  def degree(v: VIndex): Int
}

trait VertexSeqGraph[V] extends Graph {
  def vertex(v: VIndex): V
}

trait EdgeSeqGraph[E] extends Graph {
  def edge(e: EIndex): E
}

trait ElectrostaticGraph extends Graph {
  def charge(v: VIndex): Float = 1.0f
}

trait MassGraph extends Graph {
  def mass(v: VIndex): Float
}

trait EdgeWeightedGraph extends Graph {
  def edgeWeight(e: EIndex): Float
}

trait DirectedGraph extends Graph {
  def head(e: EIndex): VIndex
  def tail(e: EIndex): VIndex

  def inEdges(v: VIndex): Set[EIndex]
  def outEdges(v: VIndex): Set[EIndex]

  def endVertices(e: EIndex): Set[VIndex] = Set(head(e), tail(e))
  def edges(v: VIndex): Set[EIndex] = inEdges(v) ++ outEdges(v)
  def inDegree(v: VIndex): Int = inEdges(v).size
  def outDegree(v: VIndex): Int = outEdges(v).size
  def degree(v: VIndex): Int = inDegree(v) + outDegree(v)
}

case class ThisDirectedGraph(sourceNodes: Seq[Node], sourceEdges: Seq[Edge]) extends DirectedGraph with ElectrostaticGraph with EdgeWeightedGraph with MassGraph with EdgeSeqGraph[Edge] with VertexSeqGraph[Node] {
  def numVertices = sourceNodes.size
  def numEdges = sourceEdges.size
  def vertex(v: VIndex) = sourceNodes(v)
  def edge(e: EIndex) = sourceEdges(e)
  val nodeToIndex: Map[Node, VIndex] = sourceNodes.view.zipWithIndex.toMap
  val edgeToIndex: Map[Edge, EIndex] = sourceEdges.view.zipWithIndex.toMap
  val edgeHeadSeq: Seq[VIndex] = sourceEdges.map(edge => nodeToIndex(edge.to))
  val edgeTailSeq: Seq[VIndex] = sourceEdges.map(edge => nodeToIndex(edge.from))
  def head(e: EIndex) = edgeHeadSeq(e)
  def tail(e: EIndex) = edgeTailSeq(e)
  val inEdgesSeq: Seq[Set[EIndex]] =
    (0 until numVertices).map(v => (0 until numEdges).filter(e => head(e) == v).toSet)
  val outEdgesSeq: Seq[Set[EIndex]] =
    (0 until numVertices).map(v => (0 until numEdges).filter(e => tail(e) == v).toSet)
  def inEdges(v: VIndex) = inEdgesSeq(v)
  def outEdges(v: VIndex) = outEdgesSeq(v)
  def mass(v: VIndex): Float = sourceNodes(v).mass * (1 + degree(v) / 3)
  def edgeWeight(e: EIndex): Float = sourceEdges(e).weight
}
