package com.faacets.ariadne
package renderer

import java.awt._
import java.awt.geom.Ellipse2D

import spire.syntax.cfor._

class Node2D(val pos: Int2D, val node: Node) {
  def this(x: Int, y: Int, node: Node) =
    this(Int2D(x, y), node)
  def x = pos.x
  def y = pos.y
}

class Edge2D(val from: Node2D, val to: Node2D, val edge: Edge)

case class Viewpoint(layoutBounds: Bounds, canvasSize: Int2D,
  offset: Float2D = Float2D.zero, zoom: Float = 1.0f) {
  def canvasWidth = canvasSize.x
  def canvasHeight = canvasSize.y
  def offsetX: Float = offset.x
  def offsetY: Float = offset.y
  val scale: Float =
    Math.min(
      canvasWidth / 2 * 0.9 / Math.max(layoutBounds.maxX, Math.abs(layoutBounds.minX)),
      canvasHeight / 2 * 0.9 / Math.max(layoutBounds.maxY, Math.abs(layoutBounds.minY))).toFloat
  val c: Float = scale * zoom
  val d: Float2D = (canvasSize.toFloat :/ 2.0f) + offset
  def toGraphCoords(pt: Point): Float2D =
    (Float2D(pt.x, pt.y) - (canvasSize.toFloat :/ 2.0f) - offset) :/ c
  def toCanvasCoords(pos: Float2D): Int2D = ((pos :* c) + d).toInt

}

private[renderer] trait GraphRenderer {
  def gLayout: SpringLayout
  def graph: DirectedGraph with MassGraph with EdgeWeightedGraph with VertexSeqGraph[Node] with EdgeSeqGraph[Edge]

  private var lastCompletion: Long = System.currentTimeMillis

  def paintVertex(g2d: Graphics2D, viewpoint: Viewpoint, v: VIndex): Unit = {
    val canvasPos = viewpoint.toCanvasCoords(gLayout.pos(v))
    val n = graph.vertex(v)
    val size = Math.max(6, Math.min(30, Math.log(n.mass) + 1))
    g2d.setColor(ColorPalette.getColor(n.group))
    g2d.fill(new Ellipse2D.Double(canvasPos.x - size / 2, canvasPos.y - size / 2, size, size))
  }

  val edgeColor = new Color(198, 198, 198, 198)
  def paintEdge(g2d: Graphics2D, viewpoint: Viewpoint, e: EIndex): Unit = {
    val width = Math.min(4, Math.max(2, Math.min(8, graph.edgeWeight(e))).toInt / 2)
    g2d.setStroke(new BasicStroke(width));
    g2d.setColor(edgeColor)
    val t = graph.tail(e)
    val h = graph.head(e)
    val tPos = viewpoint.toCanvasCoords(gLayout.pos(t))
    val hPos = viewpoint.toCanvasCoords(gLayout.pos(h))
    g2d.drawLine(tPos.x, tPos.y, hPos.x, hPos.y)
  }

  def render(g2d: Graphics2D, layout: SpringLayout, selectedNode: Option[Int] = None, viewpoint: Viewpoint, showLabels: Boolean = false): Unit = {
    import viewpoint.{canvasWidth, canvasHeight, offsetX, offsetY, zoom, c, d}
    g2d.setColor(Color.WHITE)
    g2d.fillRect(0, 0, canvasWidth, canvasHeight)

    cforRange(0 until graph.numEdges)(e => paintEdge(g2d, viewpoint, e))
    cforRange(0 until graph.numVertices)(v => paintVertex(g2d, viewpoint, v))

    selectedNode.foreach { v =>
      val size = Math.log(graph.mass(v)) + 7 // TODO: display using original mass
      val p = (layout.pos(v) :* c) + d
      val n = graph.vertex(v)

      // Highlight in-links
      graph.inEdges(v).foreach { e =>
        val fromI = graph.tail(e)
        val fromN = graph.vertex(fromI)
        val from = (layout.pos(fromI) :* c.toFloat) + d
        val width = Math.min(4, Math.max(2, Math.min(8, graph.edgeWeight(e))).toInt / 2)
        val edge = graph.edge(e)
        g2d.setStroke(new BasicStroke(width));
        g2d.setColor(Color.GREEN)
        g2d.drawLine(from.x.toInt, from.y.toInt, p.x.toInt, p.y.toInt)
        g2d.setColor(Color.BLACK)
        g2d.drawString(fromN.label, from.x.toInt + 5, from.y.toInt - 2)
      }

      // Highlight out-links
      graph.outEdges(v).foreach { e =>
        val toI = graph.head(e)
        val toN = graph.vertex(toI)
        val to = (layout.pos(toI) :* c.toFloat) + d
        val width = Math.min(4, Math.max(2, Math.min(8, graph.edgeWeight(e))).toInt / 2)
    
        g2d.setStroke(new BasicStroke(width));
        g2d.setColor(Color.RED)
        g2d.drawLine(p.x.toInt, p.y.toInt, to.x.toInt, to.y.toInt)
        g2d.setColor(Color.BLACK)
        g2d.drawString(toN.label, to.x.toInt + 5, to.y.toInt - 2)
      }

      g2d.setColor(Color.BLACK);
      g2d.draw(new Ellipse2D.Double(p.x - size / 2, p.y - size / 2, size, size))
      g2d.drawString(n.label, p.x.toInt + 5, p.y.toInt - 2)
    }
    
    g2d.setColor(Color.BLACK)
    g2d.drawString("%.1f".format(1000.0 / (System.currentTimeMillis - lastCompletion)) + "FPS", 2, 12)
    
    lastCompletion = System.currentTimeMillis
  }
  
}
