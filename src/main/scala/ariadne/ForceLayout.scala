package com.faacets.ariadne

import scala.collection
import scala.collection.{immutable, mutable}

import spire.syntax.cfor._
import spire.util.Opt

trait ForceLayout extends Layout {
  /** Time-step increment **/
  protected def TIMESTEP = 20

  /** Node velocity limit **/
  protected def MAX_VELOCITY = 1.0f

  def n: Int

  val posX: Array[Float]
  val posY: Array[Float]
  @inline final def posM2(v: VIndex) = posX(v)*posX(v) + posY(v)*posY(v)
  @inline final def posRM(v: VIndex) = Utils.fastInverseSquareRoot(posM2(v))
  val velX: Array[Float]
  val velY: Array[Float]
  @inline final def velM2(v: VIndex) = velX(v)*velX(v) + velY(v)*velY(v)
  @inline final def velRM(v: VIndex) = Utils.fastInverseSquareRoot(velM2(v))
  val frcX: Array[Float]
  val frcY: Array[Float]

  def vertexPosition(v: VIndex): Float2D = pos(v)

  object pos {
    def apply(v: Int): Float2D = Float2D(posX(v), posY(v))
    def update(v: Int, vec: Float2D): Unit = {
      posX(v) = vec.x
      posY(v) = vec.y
    }
  }
  object vel {
    def apply(v: Int): Float2D = Float2D(velX(v), velY(v))
    def update(v: Int, vec: Float2D): Unit = {
      velX(v) = vec.x
      velY(v) = vec.y
    }
  }
  object frc {
    def apply(v: Int): Float2D = Float2D(frcX(v), frcY(v))
    def update(v: Int, vec: Float2D): Unit = {
      frcX(v) = vec.x
      frcY(v) = vec.y
    }
  }

  var minX: Float = -1.0f
  var minY: Float = -1.0f
  var maxX: Float = 1.0f
  var maxY: Float = 1.0f
  var totalKinematicEnergy: Float = 0.0f

  def updateForces(): Unit = { }

  def postVelocitiesAndPositionsUpdate(): Unit = { }

  def updateVelocitiesAndPositions(): Unit = {
    totalKinematicEnergy = 0.0f
    cforRange(0 until n) { v =>
      val invMass = 1.0f / graph.mass(v)
      val accX = frcX(v) * invMass
      val accY = frcY(v) * invMass
      frcX(v) = 0.0f
      frcY(v) = 0.0f
      velX(v) += accX * TIMESTEP
      velY(v) += accY * TIMESTEP
      val vm2 = velM2(v)
      if (vm2 > MAX_VELOCITY * MAX_VELOCITY) {
        val factor = velRM(v)
        velX(v) = velX(v) * factor * MAX_VELOCITY
        velY(v) = velY(v) * factor * MAX_VELOCITY
      }
      totalKinematicEnergy += 0.5f * graph.mass(v) * vm2
      posX(v) += velX(v) * TIMESTEP
      posY(v) += velY(v) * TIMESTEP
      val x = posX(v)
      val y = posY(v)
      if (v == 0) {
        minX = x
        maxX = x
        minY = y
        maxY = y
      } else {
        minX = scala.math.min(minX, x)
        minY = scala.math.min(minY, y)
        maxX = scala.math.max(maxX, x)
        maxY = scala.math.max(maxY, y)
      }
    }
    postVelocitiesAndPositionsUpdate()
  }

  def step(): Unit = {
    updateForces()
    updateVelocitiesAndPositions()
  }
}
