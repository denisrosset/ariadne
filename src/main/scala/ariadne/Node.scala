package com.faacets.ariadne

/**
 * A node in the force layout simulation. The node has an immutable component, representing the actual 
 * graph node, and a mutable 'state' field, containing the force simulation state. 
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
case class Node(id: String, label: String, mass: Float = 1.0f, group: Int = 0)
