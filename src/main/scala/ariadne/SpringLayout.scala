package com.faacets.ariadne

import scala.collection.parallel.ParSeq

import spire.syntax.cfor._
import spire.util.Opt

import quadtree._

trait Layout {
  def graph: DirectedGraph[Node, Edge]
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
    } while (totalKinematicEnergy > 0.001f && it < maxIterations)
      
    if (onComplete != null)
      onComplete(it)
  }
}

/**
 * A force directed graph layout implementation. Parts of this code are ported from the Springy
 * JavaScript library (http://getspringy.com/) by Dennis Hotson. Physics model parameters are based 
 * on those used in the JavaScript libary VivaGraphJS (https://github.com/anvaka/VivaGraphJS) by 
 * Andrei Kashcha. 
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
class SpringLayout(val graph: DirectedGraph[Node, Edge]) extends Layout {
    
  /** Repulsion constant **/
  private var REPULSION = -1.2f
  
  /** 'Gravity' constant pulling towards origin **/
  private var CENTER_GRAVITY = -1e-4f
  
  /** Default spring length **/
  private var SPRING_LENGTH = 50.0f
  
  /** Spring stiffness constant **/
  private var SPRING_COEFFICIENT = 0.0002f
  
  /** Drag coefficient **/
  private var DRAG = -0.02f
  
  /** Time-step increment **/
  private val TIMESTEP = 20
  
  /** Node velocity limit **/
  private val MAX_VELOCITY = 1.0f
  
  /** Barnes-Hut Theta Threshold **/
  private val THETA = 0.8f

  val posX: Array[Float] = new Array[Float](graph.numVertices)
  val posY: Array[Float] = new Array[Float](graph.numVertices)
  val velX: Array[Float] = new Array[Float](graph.numVertices)
  val velY: Array[Float] = new Array[Float](graph.numVertices)
  val frcX: Array[Float] = new Array[Float](graph.numVertices)
  val frcY: Array[Float] = new Array[Float](graph.numVertices)

  def vertexPosition(v: VIndex): Float2D = pos(v)

  object pos {
    def apply(v: Int): Float2D = Float2D(posX(v), posY(v))
    def update(v: Int, vec: Float2D): Unit = {
      posX(v) = vec.x
      posY(v) = vec.y
    }
  }
  object vel {
    def apply(v: Int): Float2D = Float2D(velX(v), velY(v))
    def update(v: Int, vec: Float2D): Unit = {
      velX(v) = vec.x
      velY(v) = vec.y
    }
  }
  object frc {
    def apply(v: Int): Float2D = Float2D(frcX(v), frcY(v))
    def update(v: Int, vec: Float2D): Unit = {
      frcX(v) = vec.x
      frcY(v) = vec.y
    }
  }

  def initVec(v: Int): Unit = {
    pos(v) = Float2D.random(1.0f)
    vel(v) = Float2D(0, 0)
    frc(v) = Float2D(0, 0)
  }

  private def buildGraph(): Unit =
    cforRange(0 until graph.numVertices)( i => initVec(i) )

  buildGraph()

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

  def computeBarnesHut(): Unit = {
    val bodies = (0 until graph.numVertices).map(v => Body(pos(v), v))

    val quadtree = new QuadTree(bounds, bodies)
    cforRange(0 until graph.numVertices)( v => apply(v, quadtree.root) )

    def apply(v: Int, quad: Quad): Unit = {
      val s = (quad.bounds.width + quad.bounds.height) / 2
      val d = (quad.center - pos(v)).magnitude
      if (s/d > THETA) {
        // Nearby quad
        quad.children match {
          case Opt(seq) =>
            cforRange(0 until seq.size)( i => apply(v, seq(i)) )
          case _ => quad.body match {
            case Opt(b) =>
              val d = b.pos - pos(v)
              val distance = d.magnitude
              val direction = d.normalize

              if (b.index != v)
                frc(v) += direction :* (REPULSION / (distance * distance * 0.5f))
            case _ =>
          }
        }
      } else {
        // Far-away quad
        val d = quad.center - pos(v)
        val distance = d.magnitude
        val direction = d.normalize
        frc(v) += direction :* (REPULSION * quad.bodies / (distance * distance * 0.5f))
      }
    }

  }

  def computeDrag(): Unit =
    cforRange(0 until graph.numVertices) { v =>
      frc(v) += vel(v) :* DRAG
    }
  
  def computeGravity(): Unit =
    cforRange(0 until graph.numVertices) { v =>
      frc(v) += pos(v).normalize :* (CENTER_GRAVITY * graph.mass(v))
    }

  def updateForces(): Unit = {
    computeHookesLaw()
    computeBarnesHut()
    computeDrag()
    computeGravity()
  }

  def updateVelocitiesAndPositions(): Unit =
    cforRange(0 until graph.numVertices) { v =>
      val acceleration = frc(v) :/ graph.mass(v)
      frc(v) = Float2D.zero
      vel(v) += acceleration :* TIMESTEP
      if (vel(v).magnitude > MAX_VELOCITY)
        vel(v) = vel(v).normalize :* MAX_VELOCITY
      pos(v) += vel(v) :* TIMESTEP
    }

  def totalKinematicEnergy =
    (0 until graph.numVertices).aggregate(0.0f)(
      (acc, v) => acc + 0.5f * graph.mass(v) * vel(v).magnitude2,
      _ + _
    )

  def step(): Unit = {
    updateForces()
    updateVelocitiesAndPositions()
  }

  def bounds = {
    var minX = posX(0)
    var maxX = minX
    var minY = posY(0)
    var maxY = minY

    cforRange(1 until graph.numVertices) { v =>
      val x = posX(v)
      val y = posY(v)
      minX = scala.math.min(minX, x)
      minY = scala.math.min(minY, y)
      maxX = scala.math.max(maxX, x)
      maxY = scala.math.max(maxY, y)
    }
    Bounds(minX, minY, maxX, maxY)
  }
  
  def getNearestNode(pt: Float2D) = {
    var nearest = 0
    var minDistance = (pos(0) - pt).magnitude

    cforRange(1 until graph.numVertices) { v =>
      val distance = (pos(v) - pt).magnitude
      if (distance < minDistance) {
        minDistance = distance
        nearest = v
      }
    }
    nearest
  }
}
