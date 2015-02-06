package com.faacets.ariadne

import scala.collection
import scala.collection.{immutable, mutable}

import spire.syntax.cfor._
import spire.util.Opt


trait Drag extends ForceLayout {
  /** Drag coefficient **/
  protected def DRAG = -0.02f

  def computeDrag(): Unit =
    cforRange(0 until graph.numVertices) { v =>
      frcX(v) += velX(v) * DRAG
      frcY(v) += velY(v) * DRAG
    }

  override def updateForces(): Unit = {
    super.updateForces()
    computeDrag()
  }
}

