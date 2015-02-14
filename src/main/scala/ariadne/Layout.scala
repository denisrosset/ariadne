package com.faacets.ariadne

import scala.collection
import scala.collection.{immutable, mutable}

import spire.syntax.cfor._
import spire.util.Opt

trait Layout {
  def n: Int // number of vertices
  def graph: DirectedGraph
  def vertexPosition(v: VIndex): Float2D
  def step(): Unit
  def totalKinematicEnergy: Float

  def doLayout(onComplete: (Int => Unit) = null, onIteration: (Int => Unit) = null, maxIterations: Int = 1000): Unit = {
    var it = 0
    do {
      step()
      
      if (onIteration != null)
        onIteration(it)
      it += 1
    } while (totalKinematicEnergy/n > 0.000001f && it < maxIterations)
      
    if (onComplete != null)
      onComplete(it)
  }

  def getNearestNode(pt: Float2D) = {
    var nearest = 0
    var minDistance2 = (vertexPosition(0) - pt).magnitude2

    cforRange(1 until graph.numVertices) { v =>
      val distance2 = (vertexPosition(v) - pt).magnitude2
      if (distance2 < minDistance2) {
        minDistance2 = distance2
        nearest = v
      }
    }
    nearest
  }
}
