package com.faacets.ariadne

import scala.collection
import scala.collection.{immutable, mutable}

import spire.syntax.cfor._
import spire.util.Opt

trait Hookes extends ForceLayout {
  self: SpringLayout =>

  /** Default spring length **/
  protected def SPRING_LENGTH = 50.0f
  
  /** Spring stiffness constant **/
  protected def SPRING_COEFFICIENT = 0.0002f

  def computeHookesLaw(): Unit =
    cforRange(0 until graph.numEdges) { e =>
      val t = graph.tail(e)
      val h = graph.head(e)
      val tPos = pos(t)
      val hPos = pos(h)
      val d = if (hPos == tPos)
        Float2D.random(0.1f, tPos)
      else
        hPos - tPos
      val displacement = d.magnitude - SPRING_LENGTH / graph.weight(e)
      val coeff = SPRING_COEFFICIENT * displacement / d.magnitude
      val force = d :* (coeff * 0.5f)
      frc(t) += force
      frc(h) -= force
    }

  override def updateForces(): Unit = {
    super.updateForces()
    computeHookesLaw()
  }
}
