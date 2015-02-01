package at.ait.dme.forcelayout.quadtree

import at.ait.dme.forcelayout.{ Bounds, Float2D }
import spire.util.Opt

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
