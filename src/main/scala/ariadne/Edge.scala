package com.faacets.ariadne

/**
 * An edge in the force layout simulation.
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
case class Edge(from: Node, to: Node, weight: Float = 1.0f)
