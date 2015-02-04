package com.faacets.ariadne
package quadtree

import spire.util.Opt

/**
 * A body in the quadtree.
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
case class Body(pos: Float2D, index: Int)

/**
 * A quad in the quadtree.
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
case class Quad(
    bounds: Bounds, 
    center: Float2D,
    bodies: Int,
    body: Opt[Body] = Opt.empty[Body], 
    children: Opt[Seq[Quad]] = Opt.empty[Seq[Quad]])

/**
 * An immutable quadtree implementation.
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
class QuadTree(bounds: Bounds, bodies: Seq[Body]) {

  import QuadTree._
  
  val root = build(bounds, bodies)
  
  def build(bounds: Bounds, bodies: Seq[Body]): Quad = {
    if (bodies.isEmpty) {
      Quad(bounds, bounds.center, 0)
    } else if (bodies.size == 1) {
      val body = bodies.head
      Quad(bounds, body.pos, 1, Opt(body))
    } else {
      val children = subdivideBounds(bounds)
        .map(subbounds => build(subbounds, 	clipBodies(bodies, subbounds)))
      Quad(bounds, computeCenter(bodies), bodies.size, Opt.empty[Body], Opt(children))
    }
  }  

}

object QuadTree {
  
  def subdivideBounds(bounds: Bounds) = Seq(
    Bounds(bounds.minX, bounds.minY + bounds.height / 2, bounds.minX + bounds.width / 2, bounds.maxY),
    Bounds(bounds.minX + bounds.width / 2, bounds.minY + bounds.height / 2, bounds.maxX, bounds.maxY),
    Bounds(bounds.minX + bounds.width / 2, bounds.minY, bounds.maxX, bounds.minY + bounds.height / 2),
    Bounds(bounds.minX, bounds.minY, bounds.minX + bounds.width / 2, bounds.minY + bounds.height / 2))
  
  def clipBodies(bodies: Seq[Body], bounds: Bounds) = bodies.filter(b => bounds.contains(b.pos))
  
  def computeCenter[T](bodies: Seq[Body]) = {
    var x = 0.0f
    var y = 0.0f
    val it = bodies.iterator
    while(it.hasNext) {
      val body = it.next
      x += body.pos.x
      y += body.pos.y
    }
    val n = bodies.size
    Float2D(x/n,y/n)
  }
  
}
