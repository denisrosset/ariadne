package at.ait.dme.forcelayout

import at.ait.dme.forcelayout.quadtree.{ Body, Quad, QuadTree }

import scala.collection.parallel.ParSeq

import spire.syntax.cfor._

trait DirectedGraph[V, E] {
  type EIndex = Int
  type VIndex = Int
  def numVertices: Int
  def numEdges: Int
  def vertex(v: VIndex): V
  def edge(e: EIndex): E
  def head(e: EIndex): VIndex
  def tail(e: EIndex): VIndex

  def inEdges(v: VIndex): Set[EIndex]
  def outEdges(v: VIndex): Set[EIndex]

  def endVertices(e: EIndex): Set[VIndex] = Set(head(e), tail(e))
  def edges(v: VIndex): Set[EIndex] = inEdges(v) ++ outEdges(v)

  def mass(v: VIndex): Float
  def weight(e: EIndex): Float
}

case class ThisDirectedGraph(sourceNodes: Seq[Node], sourceEdges: Seq[Edge]) extends DirectedGraph[Node, Edge] {
  def numVertices = sourceNodes.size
  def numEdges = sourceEdges.size
  def vertex(v: VIndex) = sourceNodes(v)
  def edge(e: EIndex) = sourceEdges(e)
  val nodeToIndex: Map[Node, VIndex] = sourceNodes.view.zipWithIndex.toMap
  val edgeToIndex: Map[Edge, EIndex] = sourceEdges.view.zipWithIndex.toMap
  val edgeHeadSeq: Seq[VIndex] = sourceEdges.map(edge => nodeToIndex(edge.to))
  val edgeTailSeq: Seq[VIndex] = sourceEdges.map(edge => nodeToIndex(edge.from))
  def head(e: EIndex) = edgeHeadSeq(e)
  def tail(e: EIndex) = edgeTailSeq(e)
  val inEdgesSeq: Seq[Set[EIndex]] =
    (0 until numVertices).map(v => (0 until numEdges).filter(e => head(e) == v).toSet)
  val outEdgesSeq: Seq[Set[EIndex]] =
    (0 until numVertices).map(v => (0 until numEdges).filter(e => tail(e) == v).toSet)
  def inEdges(v: VIndex) = inEdgesSeq(v)
  def outEdges(v: VIndex) = outEdgesSeq(v)
  def mass(v: VIndex): Float = sourceNodes(v).mass * (1 + edges(v).size / 3)
  def weight(e: EIndex): Float = sourceEdges(e).weight
}


/**
 * A force directed graph layout implementation. Parts of this code are ported from the Springy
 * JavaScript library (http://getspringy.com/) by Dennis Hotson. Physics model parameters are based 
 * on those used in the JavaScript libary VivaGraphJS (https://github.com/anvaka/VivaGraphJS) by 
 * Andrei Kashcha. 
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
class SpringGraph(val graph: DirectedGraph[Node, Edge]) {
    
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

  def state(v: Int) = NodeState(v)

  case class NodeState(i: Int) {
    def pos: Float2D = Float2D(posX(i), posY(i))
    def pos_=(newPos: Float2D) = {
      posX(i) = newPos.x
      posY(i) = newPos.y
    }
    def velocity: Float2D = Float2D(velX(i), velY(i))
    def velocity_=(newVel: Float2D) = {
      velX(i) = newVel.x
      velY(i) = newVel.y
    }
    def force: Float2D = Float2D(frcX(i), frcY(i))
    def force_=(newFrc: Float2D) = {
      frcX(i) = newFrc.x
      frcY(i) = newFrc.y
    }
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
    val bodies = (0 until graph.numVertices).map(v => Body(NodeState(v).pos, v))

    val quadtree = new QuadTree(bounds, bodies)

    def apply(v: Int, quad: Quad): Unit = {
      val state = NodeState(v)
      val s = (quad.bounds.width + quad.bounds.height) / 2
      val d = (quad.center - state.pos).magnitude
      if (s/d > THETA) {
        // Nearby quad
        quad.children match {
          case Some(seq) =>
            seq.foreach(child => apply(v, child))
          case None => quad.body match {
            case Some(b) =>
              val d = b.pos - state.pos
              val distance = d.magnitude
              val direction = d.normalize

              if (b.index != v)
                state.force += direction :* (REPULSION / (distance * distance * 0.5f))
            case None =>
          }
        }
      } else {
        // Far-away quad
        val d = quad.center - state.pos
        val distance = d.magnitude
        val direction = d.normalize
        state.force += direction :* (REPULSION * quad.bodies / (distance * distance * 0.5f))
      }
    }

    cforRange(0 until graph.numVertices)( v => apply(v, quadtree.root) )
  }

  def computeDrag(): Unit =
    cforRange(0 until graph.numVertices) { v =>
      val state = NodeState(v)
      state.force += state.velocity :* DRAG
    }
  
  def computeGravity(): Unit =
    cforRange(0 until graph.numVertices) { v =>
      val state = NodeState(v)
      state.force += state.pos.normalize :* (CENTER_GRAVITY * graph.mass(v))
    }

  def updateForces(): Unit = {
    computeHookesLaw()
    computeBarnesHut()
    computeDrag()
    computeGravity()
  }

  def updateVelocitiesAndPositions(): Unit =
    cforRange(0 until graph.numVertices) { v =>
      val state = NodeState(v)
      val acceleration = NodeState(v).force :/ graph.mass(v)
      state.force = Float2D.zero
      state.velocity += acceleration :* TIMESTEP
      if (state.velocity.magnitude > MAX_VELOCITY)
        state.velocity = state.velocity.normalize :* MAX_VELOCITY
      state.pos += state.velocity :* TIMESTEP
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
