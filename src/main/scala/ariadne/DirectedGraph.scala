package com.faacets.ariadne


import scala.collection.parallel.ParSeq

import spire.syntax.cfor._
import spire.util.Opt

import quadtree._

trait DirectedGraph[V, E] {
  def numVertices: Int
  def numEdges: Int
  def vertex(v: VIndex): V
  def edge(e: EIndex): E
  def head(e: EIndex): VIndex
  def tail(e: EIndex): VIndex

  def inEdges(v: VIndex): Set[EIndex]
  def outEdges(v: VIndex): Set[EIndex]

  def endVertices(e: EIndex): Set[VIndex] = Set(head(e), tail(e))
  def edges(v: VIndex): Set[EIndex] = inEdges(v) ++ outEdges(v)
  def degree(v: VIndex): Int = inEdges(v).size + outEdges(v).size

  def mass(v: VIndex): Float
  def weight(e: EIndex): Float
}

case class ThisDirectedGraph(sourceNodes: Seq[Node], sourceEdges: Seq[Edge]) extends DirectedGraph[Node, Edge] {
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
  def weight(e: EIndex): Float = sourceEdges(e).weight
}
