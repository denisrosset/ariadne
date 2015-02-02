package com.faacets.ariadne
package renderer

import java.awt._
import java.awt.geom.Ellipse2D

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
  def graph: DirectedGraph[Node, Edge] = gLayout.graph

  private var lastCompletion: Long = System.currentTimeMillis

  private var nodePainter = (nodes: Seq[Node2D], g2d: Graphics2D) => {
    nodes.foreach(n2d => {
      val (x, y, n) = (n2d.x, n2d.y, n2d.node)
      val size = Math.max(6, Math.min(30, Math.log(n.mass) + 1))
      g2d.setColor(ColorPalette.getColor(n.group))
      g2d.fill(new Ellipse2D.Double(x - size / 2, y - size / 2, size, size))
    })
  }
  
  def setNodePainter(painter: (Seq[Node2D], Graphics2D) => Unit) =
    nodePainter = painter
  
  private var edgePainter = (edges: Seq[Edge2D], g2d: Graphics2D) => {
    edges.foreach(e2d => {
      val width = Math.min(4, Math.max(2, Math.min(8, e2d.edge.weight)).toInt / 2)   
      g2d.setStroke(new BasicStroke(width));
      g2d.setColor(new Color(198, 198, 198, 198))  
      g2d.drawLine(e2d.from.x, e2d.from.y, e2d.to.x, e2d.to.y)
    })
  } 
  
  def setEdgePainter(painter: (Seq[Edge2D], Graphics2D) => Unit) =
    edgePainter = painter

  def render(g2d: Graphics2D, layout: SpringLayout, selectedNode: Option[Int] = None, viewpoint: Viewpoint, showLabels: Boolean = false): Unit = {
    val graph = layout.graph
    import viewpoint.{canvasWidth, canvasHeight, offsetX, offsetY, zoom, c, d}
    g2d.setColor(Color.WHITE)
    g2d.fillRect(0, 0, canvasWidth, canvasHeight)

    val edges2D = (0 until graph.numEdges).map(e => {
      val edge = graph.edge(e)
      val fromI = graph.tail(e)
      val toI = graph.head(e)
      val fromV = graph.vertex(fromI)
      val toV = graph.vertex(toI)

      val from = new Node2D(((layout.pos(fromI) :* c) + d).toInt, fromV)
      val to = new Node2D(((layout.pos(toI) :* c) + d).toInt, toV)
      new Edge2D(from, to, edge)
    })
    edgePainter(edges2D, g2d)
    
    val nodes2D = (0 until graph.numVertices).map { v =>
      val n = graph.vertex(v)
      new Node2D(((layout.pos(v) :* c) + d).toInt, n)
    }
      .filter(n2d => n2d.x > 0 && n2d.y > 0)
      .filter(n2d => n2d.x <= canvasWidth && n2d.y <= canvasHeight)

    nodePainter(nodes2D, g2d)
    
    if (showLabels) {
      g2d.setColor(Color.BLACK)
      nodes2D.foreach(n2d => g2d.drawString(n2d.node.label, n2d.x + 5, n2d.y - 2))
    }  

    selectedNode.foreach { v =>
      val size = Math.log(graph.mass(v)) + 7 // TODO: display using original mass
      val p = (layout.pos(v) :* c) + d
      val n = graph.vertex(v)

      // Highlight in-links
      graph.inEdges(v).foreach { e =>
        val fromI = graph.tail(e)
        val fromN = graph.vertex(fromI)
        val from = (layout.pos(fromI) :* c.toFloat) + d
        val width = Math.min(4, Math.max(2, Math.min(8, graph.weight(e))).toInt / 2)
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
        val width = Math.min(4, Math.max(2, Math.min(8, graph.weight(e))).toInt / 2)
    
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
