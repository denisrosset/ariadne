package at.ait.dme.forcelayout.quadtree

import at.ait.dme.forcelayout.{ Bounds, Float2D }

/**
 * A quad in the quadtree.
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
case class Quad(
    bounds: Bounds, 
    center: Float2D,
    bodies: Int,
    body: Option[Body] = None, 
    children: Option[Seq[Quad]] = None)
