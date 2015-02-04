package com.faacets.ariadne

import scala.collection
import scala.collection.{immutable, mutable}

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

  sealed trait QuadTree {
    def minX: Float
    def minY: Float
    def maxX: Float
    def maxY: Float
    def width = maxX - minX
    def height = maxY - minY
    // TODO: compute the real center of mass
    def center = Float2D((minX + maxX) / 2, (minY + maxY) / 2)
    def bodies: Int
  }

  sealed trait QuadTreeBuilder extends QuadTree { self =>
    def withPoint(v: VIndex): QuadBranchBuilder
  }

  sealed trait QuadBranch {
    def tl: Opt[QuadTree] // top left
    def tr: Opt[QuadTree] // top right
    def bl: Opt[QuadTree] // bottom left
    def br: Opt[QuadTree] // bottom right
  }

  sealed trait QuadLeaf extends QuadTree {
    def v: VIndex
    def bodies = 1
  }

  final class QuadBranchBuilder(
    val minX: Float,
    val minY: Float,
    val maxX: Float,
    val maxY: Float,
    var bodies: Int = 0,
    var tl: Opt[QuadTreeBuilder] = Opt.empty[QuadTreeBuilder],
    var tr: Opt[QuadTreeBuilder] = Opt.empty[QuadTreeBuilder],
    var bl: Opt[QuadTreeBuilder] = Opt.empty[QuadTreeBuilder],
    var br: Opt[QuadTreeBuilder] = Opt.empty[QuadTreeBuilder]
  ) extends QuadBranch with QuadTreeBuilder {
    require(minX < maxX)
    require(minY < maxY)
    val midX = (minX + maxX) / 2
    val midY = (minY + maxY) / 2
    def withPoint(w: VIndex): QuadBranchBuilder = {
      bodies += 1
      val verX = posX(w)
      val verY = posY(w)
      if (verY < midY) { // top
        if (verX < midX) // left
          tl = Opt(tl.fold(new QuadLeafBuilder(minX, minY, midX, midY, w): QuadTreeBuilder)(_.withPoint(w)))
        else // right
          tr = Opt(tr.fold(new QuadLeafBuilder(midX, minY, maxX, midY, w): QuadTreeBuilder)(_.withPoint(w)))
      } else { // bottom
        if (verX < midX) // left
          bl = Opt(bl.fold(new QuadLeafBuilder(minX, midY, midX, maxY, w): QuadTreeBuilder)(_.withPoint(w)))
        else
          br = Opt(br.fold(new QuadLeafBuilder(midX, midY, maxX, maxY, w): QuadTreeBuilder)(_.withPoint(w)))
      }
      this
    }
  }

  final class QuadLeafBuilder(
    var minX: Float,
    var minY: Float,
    var maxX: Float,
    var maxY: Float,
    var v: VIndex) extends QuadLeaf with QuadTreeBuilder {
    require(minX < maxX)
    require(minY < maxY)
    def midX: Float = (minX + maxX) / 2
    def midY: Float = (minY + maxY) / 2
    def subdivided: QuadBranchBuilder = {
      val replacedBy = new QuadBranchBuilder(minX, minY, maxX, maxY, 1)
      val verX = posX(v)
      val verY = posY(v)
      if (verY < midY) { // top
        maxY = midY
        if (verX < midX) { // left
          maxX = midX
          replacedBy.tl = Opt(this)
        } else { // right
          minX = midX
          replacedBy.tr = Opt(this)
        }
      } else { // bottom
        minY = midY
        if (verX < midX) { // left
          maxX = midX
          replacedBy.bl = Opt(this)
        } else { // right
          minX = midX
          replacedBy.br = Opt(this)
        }
      }
      replacedBy
    }

    def withPoint(w: VIndex): QuadBranchBuilder = subdivided.withPoint(w)
  }


  def getBounds = Bounds(minX, minY, maxX, maxY)

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

  var minX: Float = -1.0f
  var minY: Float = -1.0f
  var maxX: Float = 1.0f
  var maxY: Float = 1.0f

  var newQT: QuadTree = _

  def updateQuadTree(): Unit = {
    var newQuad: QuadTreeBuilder = new QuadBranchBuilder(minX, minY, maxX, maxY)
    cforRange(0 until graph.numVertices) { v =>
      newQuad = newQuad.withPoint(v)
    }
    newQT = newQuad
  }

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

    val qt = new quadtree.QuadTree(getBounds, bodies)
//    cforRange(0 until graph.numVertices)( v => applyOld(v, qt.root) )
    cforRange(0 until graph.numVertices)( v => applyNew(v, newQT) )

    def applyOld(v: Int, quad: Quad): Unit = {
      val s = (quad.bounds.width + quad.bounds.height) / 2
      val d = (quad.center - pos(v)).magnitude
      if (s/d > THETA) {
        // Nearby quad
        quad.children match {
          case Opt(seq) =>
            cforRange(0 until seq.size)( i => applyOld(v, seq(i)) )
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

    def applyNew(v: Int, quad: QuadTree): Unit = {
      val s = (quad.width + quad.height) / 2
      val d = (quad.center - pos(v)).magnitude
      quad match {
        case ql: QuadLeaf if ql.v != v =>
          val d = pos(ql.v) - pos(v)
          val distance = d.magnitude
          val direction = d.normalize
          frc(v) += direction :* (REPULSION / (distance * distance * 0.5f))
        case qb: QuadBranch if s/d > THETA => // nearby quad
          if (qb.tl.nonEmpty) applyNew(v, qb.tl.get)
          if (qb.tr.nonEmpty) applyNew(v, qb.tr.get)
          if (qb.bl.nonEmpty) applyNew(v, qb.bl.get)
          if (qb.br.nonEmpty) applyNew(v, qb.br.get)
        case qb: QuadBranch => // far-away quad
          val d = qb.center - pos(v)
          val distance = d.magnitude
          val direction = d.normalize
          frc(v) += direction :* (REPULSION * quad.bodies / (distance * distance * 0.5f))
        case _ =>
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

  def updateVelocitiesAndPositions(): Unit = {
    cforRange(0 until graph.numVertices) { v =>
      val acceleration = frc(v) :/ graph.mass(v)
      frc(v) = Float2D.zero
      vel(v) += acceleration :* TIMESTEP
      if (vel(v).magnitude > MAX_VELOCITY)
        vel(v) = vel(v).normalize :* MAX_VELOCITY
      pos(v) += vel(v) :* TIMESTEP
      val x = posX(v)
      val y = posY(v)
      if (v == 0) {
        minX = x
        maxX = x
        minY = y
        maxY = y
      } else {
        minX = scala.math.min(minX, x)
        minY = scala.math.min(minY, y)
        maxX = scala.math.max(maxX, x)
        maxY = scala.math.max(maxY, y)
      }
    }
    updateQuadTree()
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
