package com.faacets.ariadne

import scala.collection
import scala.collection.{immutable, mutable}

import spire.syntax.cfor._
import spire.util.Opt

/**
  * A force directed graph layout implementation. Parts of this code are ported from the Springy
  * JavaScript library (http://getspringy.com/) by Dennis Hotson. Physics model parameters are based 
  * on those used in the JavaScript libary VivaGraphJS (https://github.com/anvaka/VivaGraphJS) by 
  * Andrei Kashcha. 
  * @author Rainer Simon <rainer.simon@ait.ac.at>
  */
class SpringLayout(val graph: DirectedGraph[Node, Edge]) extends ForceLayout with Electrostatic with Hookes with Gravity with Drag {

  val n = graph.numVertices

  val posX: Array[Float] = new Array[Float](n)
  val posY: Array[Float] = new Array[Float](n)
  val velX: Array[Float] = new Array[Float](n)
  val velY: Array[Float] = new Array[Float](n)
  val frcX: Array[Float] = new Array[Float](n)
  val frcY: Array[Float] = new Array[Float](n)
  
  def getBounds = Bounds(minX, minY, maxX, maxY)

  def initVec(v: Int): Unit = {
    pos(v) = Float2D.random(1.0f)
    vel(v) = Float2D(0, 0)
    frc(v) = Float2D(0, 0)
  }

  private def buildGraph(): Unit = {
    cforRange(0 until graph.numVertices)( i => initVec(i) )
    updateQuadTree()
  }

  buildGraph()

}
