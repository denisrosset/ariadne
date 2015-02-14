package com.faacets.ariadne

/**
 * A node in the force layout simulation. The node has an immutable component, representing the actual graph node.
  * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
case class Node(label: String, mass: Float = 1.0f, group: Int = 0)
