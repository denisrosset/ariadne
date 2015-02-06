package com.faacets.ariadne

import scala.collection
import scala.collection.{immutable, mutable}

import spire.syntax.cfor._
import spire.util.Opt

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
            val dX = posX(ql.v) - posX(v)
            val dY = posY(ql.v) - posY(v)
            val dm2 = dX*dX + dY*dY
            val dm = Utils.fastSquareRoot(dm2)
            val factor = REPULSION / (dm * dm2 * 0.5f)
            frcX(v) += dX * factor
            frcY(v) += dY * factor
          }
        case qb: QuadBranch =>
          val s = (qb.width + qb.height) / 2
          val dX = qb.comX - posX(v)
          val dY = qb.comY - posY(v)
          val dm2 = dX*dX + dY*dY

          if (s*s/dm2 > THETA*THETA) { // nearby quad
            if (qb.tl.nonEmpty) apply(v, qb.tl.get)
            if (qb.tr.nonEmpty) apply(v, qb.tr.get)
            if (qb.bl.nonEmpty) apply(v, qb.bl.get)
            if (qb.br.nonEmpty) apply(v, qb.br.get)
          } else {
            val dm = Utils.fastSquareRoot(dm2)
            val factor = REPULSION * quad.bodies / (dm * dm2 * 0.5f)
            frcX(v) += dX * factor
            frcY(v) += dY * factor
          }
      }
    }
  }
}

