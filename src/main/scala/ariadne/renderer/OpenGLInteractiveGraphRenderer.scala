package com.faacets.ariadne
package renderer

import java.awt.{ Canvas, Graphics2D, Point, RenderingHints }
import java.awt.image.BufferStrategy
import java.awt.event.{ MouseAdapter, MouseEvent, MouseWheelListener, MouseWheelEvent }

class OpenGLInteractiveGraphRenderer(val gLayout: SpringLayout) extends Canvas with GraphRenderer {
  
  System.setProperty("sun.java2d.opengl", "True")
  System.setProperty("sun.java2d.ddscale", "True")
  System.setProperty("sun.java2d.translaccel", "True")  
    
  private var currentZoom = 1.0f
  private var currentXOffset = 0.0f
  private var currentYOffset = 0.0f
  private var lastMousePos = new Point(0, 0)
  
  private var selectedNode: Option[Int] = None
  
  private var strategy: BufferStrategy = null
  
  addMouseMotionListener(new MouseAdapter() {
    override def mouseDragged(e: MouseEvent) {
      currentXOffset += (e.getX - lastMousePos.getX).toFloat
      currentYOffset += (e.getY - lastMousePos.getY).toFloat
      lastMousePos = e.getPoint
      doPaint(strategy)
    }
  })
  
  addMouseListener(new MouseAdapter() {
    override def mouseClicked(e: MouseEvent) {
      val size = getSize()
      val viewpoint = Viewpoint(gLayout.getBounds, Int2D(size.getWidth.toInt, size.getHeight.toInt), Float2D(currentXOffset, currentYOffset), currentZoom)
      val coords = viewpoint.toGraphCoords(e.getPoint)
      selectedNode = Some(gLayout.getNearestNode(coords))
      doPaint(strategy)
    }
    
    override def mousePressed(e: MouseEvent) = lastMousePos = e.getPoint 
  })
  
  addMouseWheelListener(new MouseWheelListener() {
    override def mouseWheelMoved(e: MouseWheelEvent) {
      // TODO make zooming sensitive to mouse position
      if (e.getWheelRotation() > 0)
        currentZoom /= 1.1f
      else
        currentZoom *= 1.1f
        
      doPaint(strategy)
    }
  })
  
  def start = {
    createBufferStrategy(2)
    strategy = getBufferStrategy
    gLayout.doLayout(onComplete = (it => { println("completed in " + it + " iterations"); doPaint(strategy) }),
                   onIteration = (it => doPaint(strategy))) 
  }

  def doPaint(strategy: BufferStrategy): Unit = {
    val g2d = strategy.getDrawGraphics.asInstanceOf[Graphics2D]
    g2d.setRenderingHint(
        RenderingHints.KEY_ANTIALIASING,
        RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setRenderingHint(
        RenderingHints.KEY_FRACTIONALMETRICS,
        RenderingHints.VALUE_FRACTIONALMETRICS_ON)
    
    val bounds = getSize
    val bnds = Int2D(bounds.getWidth.toInt, bounds.getHeight.toInt)
    val viewpoint = Viewpoint(gLayout.getBounds, bnds, Float2D(currentXOffset, currentYOffset), currentZoom)
    render(g2d, gLayout, selectedNode, viewpoint)
    g2d.dispose
    strategy.show
  }

}
