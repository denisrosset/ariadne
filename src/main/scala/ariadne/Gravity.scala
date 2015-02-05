package com.faacets.ariadne

import scala.collection
import scala.collection.{immutable, mutable}

import spire.syntax.cfor._
import spire.util.Opt

trait Gravity extends ForceLayout {
  self: SpringLayout =>

  /** 'Gravity' constant pulling towards origin **/
  protected def CENTER_GRAVITY = -1e-4f

  def computeGravity(): Unit =
    cforRange(0 until graph.numVertices) { v =>
      frc(v) += pos(v).normalize :* (CENTER_GRAVITY * graph.mass(v))
    }

  override def updateForces(): Unit = {
    super.updateForces()
    computeGravity()
  }
}
