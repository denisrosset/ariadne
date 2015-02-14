package com.faacets.ariadne

import scala.collection
import scala.collection.{immutable, mutable}

import spire.syntax.cfor._
import spire.util.Opt

/**
  * A force directed graph layout implementation. Parts of this code are ported from the Springy
  * JavaScript library (http://getspringy.com/) by Dennis Hotson. Physics model parameters are based 
  * on those used in the JavaScript libary VivaGraphJS (https://github.com/anvaka/VivaGraphJS) by 
  * Andrei Kashcha. 
  * @author Rainer Simon <rainer.simon@ait.ac.at>
  */
class SpringLayout(val graph: DirectedGraph with ElectrostaticGraph with MassGraph with EdgeWeightedGraph) extends ForceLayout with Electrostatic with Hookes with Gravity with Drag
