package com.faacets.ariadne

import scala.collection
import scala.collection.{immutable, mutable}

import spire.syntax.cfor._
import spire.util.Opt

trait Hookes extends ForceLayout {
  def graph: DirectedGraph with MassGraph with EdgeWeightedGraph

  /** Default spring length **/
  protected def SPRING_LENGTH = 50.0f
  
  /** Spring stiffness constant **/
  protected def SPRING_COEFFICIENT = 0.0002f

  def computeHookesLaw(): Unit =
    cforRange(0 until graph.numEdges) { e =>
      val t = graph.tail(e)
      val h = graph.head(e)
      val tX = posX(t)
      val tY = posY(t)
      val hX = posX(h)
      val hY = posY(h)
      var dX = 0.0f
      var dY = 0.0f
      if (hX == tX && hY == tY) {
        val d = Float2D.random(0.1f, Float2D(tX, tY))
        dX = d.x
        dY = d.y
      } else {
        dX = hX - tX
        dY = hY - tY
      }
      val dM2 = dX*dX + dY*dY
      val dRM = Utils.fastInverseSquareRoot(dM2)
      val dM = dM2*dRM
      val disp = dM - SPRING_LENGTH / graph.edgeWeight(e)
      val coeff = SPRING_COEFFICIENT * disp * dRM
      val forceX = dX * (coeff * 0.5f)
      val forceY = dY * (coeff * 0.5f)
      frcX(t) += forceX
      frcY(t) += forceY
      frcX(h) -= forceX
      frcY(h) -= forceY
    }

  override def updateForces(): Unit = {
    super.updateForces()
    computeHookesLaw()
  }
}
