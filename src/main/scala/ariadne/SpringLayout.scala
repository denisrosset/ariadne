package com.faacets.ariadne

import scala.collection
import scala.collection.{immutable, mutable}

import spire.syntax.cfor._
import spire.util.Opt

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

trait ForceLayout extends Layout {
  /** Time-step increment **/
  protected def TIMESTEP = 20

  /** Node velocity limit **/
  protected def MAX_VELOCITY = 1.0f

  def n: Int

  val posX: Array[Float]
  val posY: Array[Float]
  val velX: Array[Float]
  val velY: Array[Float]
  val frcX: Array[Float]
  val frcY: Array[Float]

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
  var totalKinematicEnergy: Float = 0.0f

  def updateForces(): Unit = { }

  def postVelocitiesAndPositionsUpdate(): Unit = { }

  def updateVelocitiesAndPositions(): Unit = {
    totalKinematicEnergy = 0.0f
    cforRange(0 until n) { v =>
      val acceleration = frc(v) :/ graph.mass(v)
      frc(v) = Float2D.zero
      vel(v) += acceleration :* TIMESTEP
      val vm2 = vel(v).magnitude2
      if (vm2 > MAX_VELOCITY * MAX_VELOCITY)
        vel(v) = vel(v).normalize :* MAX_VELOCITY
      totalKinematicEnergy += 0.5f * graph.mass(v) * vm2
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
    postVelocitiesAndPositionsUpdate()
  }

  def step(): Unit = {
    updateForces()
    updateVelocitiesAndPositions()
  }
}


trait Electrostatic extends ForceLayout {
  self: SpringLayout =>

  /** Repulsion constant **/
  protected def REPULSION = -1.2f

  /** Barnes-Hut Theta Threshold **/
  protected def THETA = 0.8f

  sealed trait QuadTree {
    def minX: Float
    def minY: Float
    def maxX: Float
    def maxY: Float
    def comX: Float // center of "mass"
    def comY: Float
    def com: Float2D = Float2D(comX, comY)
    def width = maxX - minX
    def height = maxY - minY
    // TODO: compute the real center of mass
    def center = Float2D((minX + maxX) / 2, (minY + maxY) / 2)
    def bodies: Int
  }

  sealed trait QuadTreeBuilder extends QuadTree { self =>
    def withPoint(v: VIndex): QuadBranchBuilder
    def computeCom(): Unit
    def result(): QuadTree = {
      computeCom()
      this
    }
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
    var comX: Float = 0.0f,
    var comY: Float = 0.0f,
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
          tl = Opt(tl match {
            case Opt(q) => q.withPoint(w)
            case _ => new QuadLeafBuilder(minX, minY, midX, midY, w)
          })
          else // right
            tr = Opt(tr match {
              case Opt(q) => q.withPoint(w)
              case _ => new QuadLeafBuilder(midX, minY, maxX, midY, w)
            })
      } else { // bottom
        if (verX < midX) // left
          bl = Opt(bl match {
            case Opt(q) => q.withPoint(w)
            case _ => new QuadLeafBuilder(minX, midY, midX, maxY, w)
          })
          else
            br = Opt(br match {
              case Opt(q) => q.withPoint(w)
              case _ => new QuadLeafBuilder(midX, midY, maxX, maxY, w)
            })
      }
      this
    }
    def computeCom(): Unit = {
      def getWeighted(qOpt: Opt[QuadTreeBuilder]): Float2D = qOpt match {
        case Opt(q) =>
          q.computeCom()
          Float2D(q.comX, q.comY) :* q.bodies
        case _ => Float2D.zero
      }
      val weighted = getWeighted(tl) + getWeighted(tr) + getWeighted(bl) + getWeighted(br)
      val com = weighted :/ bodies
      comX = com.x
      comY = com.y
    }
  }

  final class QuadLeafBuilder(
    var minX: Float,
    var minY: Float,
    var maxX: Float,
    var maxY: Float,
    var v: VIndex) extends QuadLeaf with QuadTreeBuilder {
    def comX = posX(v)
    def comY = posY(v)
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
    def computeCom(): Unit = { } // do nothing
  }

  var newQT: QuadTree = _

  def updateQuadTree(): Unit = {
    var newQuad: QuadTreeBuilder = new QuadBranchBuilder(minX, minY, maxX, maxY)
    cforRange(0 until graph.numVertices) { v =>
      newQuad = newQuad.withPoint(v)
    }
    newQT = newQuad.result()
  }

  override def postVelocitiesAndPositionsUpdate(): Unit = {
    super.postVelocitiesAndPositionsUpdate()
    updateQuadTree()
  }

  override def updateForces(): Unit = {
    super.updateForces()
    computeBarnesHut()
  }

  def computeBarnesHut(): Unit = {
    cforRange(0 until graph.numVertices)( v => apply(v, newQT) )

    def apply(v: Int, quad: QuadTree): Unit = {
      quad match {
        case ql: QuadLeaf =>
          if (ql.v != v) {
            val d = pos(ql.v) - pos(v)
            val distance2 = d.magnitude2
            val direction = d.normalize
            frc(v) += direction :* (REPULSION / (distance2 * 0.5f))
          }
        case qb: QuadBranch =>
          val s = (qb.width + qb.height) / 2
          val d = qb.com - pos(v)
          val dm2 = d.magnitude2

          if (s*s/dm2 > THETA*THETA) { // nearby quad
            if (qb.tl.nonEmpty) apply(v, qb.tl.get)
            if (qb.tr.nonEmpty) apply(v, qb.tr.get)
            if (qb.bl.nonEmpty) apply(v, qb.bl.get)
            if (qb.br.nonEmpty) apply(v, qb.br.get)
          } else {
            val dm = d.magnitude
            frc(v) += d :* (REPULSION * quad.bodies / (dm * dm2 * 0.5f))
          }
      }
    }
  }
}

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

  def getNearestNode(pt: Float2D) = {
    var nearest = 0
    var minDistance2 = (pos(0) - pt).magnitude2

    cforRange(1 until graph.numVertices) { v =>
      val distance2 = (pos(v) - pt).magnitude2
      if (distance2 < minDistance2) {
        minDistance2 = distance2
        nearest = v
      }
    }
    nearest
  }
}


