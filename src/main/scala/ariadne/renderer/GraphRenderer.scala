package at.ait.dme.forcelayout.renderer

import java.awt._
import java.awt.geom.Ellipse2D
import at.ait.dme.forcelayout.{ Node, Edge, SpringGraph, Float2D, Int2D }

class Node2D(val pos: Int2D, val node: Node) {
  def this(x: Int, y: Int, node: Node) =
    this(Int2D(x, y), node)
  def x = pos.x
  def y = pos.y
}

class Edge2D(val from: Node2D, val to: Node2D, val edge: Edge)

private[renderer] trait GraphRenderer {

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
    
  def render(g2d: Graphics2D, graph: SpringGraph, width: Int, height: Int, selectedNode: Option[Int] = None, offsetX: Float = 0.0f, offsetY: Float = 0.0f, zoom: Float = 1.0f, showLabels: Boolean = false): Unit = {
    g2d.setColor(Color.WHITE)
    g2d.fillRect(0, 0, width, height)

    val c = computeScale(graph, width, height) * zoom
    val (dx, dy) = (width / 2 + offsetX, height / 2 + offsetY)
    
    val edges2D = (0 until graph.graph.numEdges).map(e => {
      val edge = graph.graph.edge(e)
      val fromI = graph.graph.tail(e)
      val toI = graph.graph.head(e)
      val fromV = graph.graph.vertex(fromI)
      val toV = graph.graph.vertex(toI)

      val from = new Node2D((c * graph.pos(fromI).x + dx).toInt, (c * graph.pos(fromI).y + dy).toInt, fromV)
      val to = new Node2D((c * graph.pos(toI).x + dx).toInt, (c * graph.pos(toI).y + dy).toInt, toV)
      new Edge2D(from, to, edge)
    })
    edgePainter(edges2D, g2d)
    
    val nodes2D = (0 until graph.graph.numVertices).map { v =>
      val n = graph.graph.vertex(v)
      new Node2D((c * graph.pos(v).x + dx).toInt, (c * graph.pos(v).y + dy).toInt, n)
    }
      .filter(n2d => n2d.x > 0 && n2d.y > 0)
      .filter(n2d => n2d.x <= width && n2d.y <= height)

    nodePainter(nodes2D, g2d)
    
    if (showLabels) {
      g2d.setColor(Color.BLACK)
      nodes2D.foreach(n2d => g2d.drawString(n2d.node.label, n2d.x + 5, n2d.y - 2))
    }  

    selectedNode.foreach { v =>
      val size = Math.log(graph.graph.mass(v)) + 7 // TODO: display using original mass
      val px = c * graph.posX(v) + dx 
      val py = c * graph.posY(v) + dy
      val n = graph.graph.vertex(v)

      // Highlight in-links
      graph.graph.inEdges(v).foreach { e =>
        val fromI = graph.graph.tail(e)
        val fromN = graph.graph.vertex(fromI)
        val from = (graph.pos(fromI) :* c.toFloat) + Float2D(dx, dy)
        val width = Math.min(4, Math.max(2, Math.min(8, graph.graph.weight(e))).toInt / 2)
        val edge = graph.graph.edge(e)
        g2d.setStroke(new BasicStroke(width));
        g2d.setColor(Color.GREEN)
        g2d.drawLine(from.x.toInt, from.y.toInt, px.toInt, py.toInt)
        g2d.setColor(Color.BLACK)
        g2d.drawString(fromN.label, from.x.toInt + 5, from.y.toInt - 2)
      }

      // Highlight out-links
      graph.graph.outEdges(v).foreach { e =>
        val toI = graph.graph.head(e)
        val toN = graph.graph.vertex(toI)
        val to = (graph.pos(toI) :* c.toFloat) + Float2D(dx, dy)
        val width = Math.min(4, Math.max(2, Math.min(8, graph.graph.weight(e))).toInt / 2)
    
        g2d.setStroke(new BasicStroke(width));
        g2d.setColor(Color.RED)
        g2d.drawLine(px.toInt, py.toInt, to.x.toInt, to.y.toInt)
        g2d.setColor(Color.BLACK)
        g2d.drawString(toN.label, to.x.toInt + 5, to.y.toInt - 2)
      }

      g2d.setColor(Color.BLACK);
      g2d.draw(new Ellipse2D.Double(px - size / 2, py - size / 2, size, size))
      g2d.drawString(n.label, px.toInt + 5, py.toInt - 2)
    }
    
    g2d.setColor(Color.BLACK)
    g2d.drawString("%.1f".format(1000.0 / (System.currentTimeMillis - lastCompletion)) + "FPS", 2, 12)
    
    lastCompletion = System.currentTimeMillis
  }
  
  private def computeScale(graph: SpringGraph, width: Int, height: Int) = {
    val bounds = graph.bounds
    Math.min(width / 2 * 0.9 / Math.max(bounds.maxX, Math.abs(bounds.minX)), height / 2 * 0.9 / Math.max(bounds.maxY, Math.abs(bounds.minY)))    
  }
        
  def toGraphCoords(graph: SpringGraph, pt: Point, width: Int, height: Int, offsetX: Float = 0.0f, offsetY: Float = 0.0f, zoom: Float = 1.0f): Float2D = {
    val c = computeScale(graph, width, height)
    val gx = (pt.x - width / 2 - offsetX) / (c * zoom)
    val gy = (pt.y - height / 2 - offsetY) / (c * zoom) 
    Float2D(gx, gy)
  }
  
}
