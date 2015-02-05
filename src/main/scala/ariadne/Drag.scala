package com.faacets.ariadne

import scala.collection
import scala.collection.{immutable, mutable}

import spire.syntax.cfor._
import spire.util.Opt


trait Drag extends ForceLayout {
  self: SpringLayout =>

  /** Drag coefficient **/
  protected def DRAG = -0.02f

  def computeDrag(): Unit =
    cforRange(0 until graph.numVertices) { v =>
      frc(v) += vel(v) :* DRAG
    }

  override def updateForces(): Unit = {
    super.updateForces()
    computeDrag()
  }
}

