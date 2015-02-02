package com.faacets.ariadne

import scala.util.Random

/** Value class encoding a Int 2D vecotr in a Long. */
class Int2D(val u: Long) extends AnyVal {
  override final def toString: String = "(%s, %s)" format (x, y)

  final def x: Int = FastInt2D.x(u)
  final def y: Int = FastInt2D.y(u)

  final def negate: Int2D = new Int2D(FastInt2D.negate(u))

  final def +(b: Float2D) = new Float2D(FastFloat2D.add(u, b.u))
  final def -(b: Float2D) = new Float2D(FastFloat2D.subtract(u, b.u))
}

object Int2D {
  import FastInt2D.encode

  @inline final def apply(x: Int, y: Int): Int2D =
    new Int2D(encode(x, y))

  @inline final def zero = apply(0, 0)
  @inline final def ex = apply(1, 0)
  @inline final def ey = apply(0, 1)
}

object FastInt2D {
  // get the x part of the 2D vector
  @inline final def x(d: Long): Int = (d & 0xffffffff).toInt

  // get the y part of the 2D vector
  @inline final def y(d: Long): Int = (d >>> 32).toInt

  // encode two floats representing a vector
  @inline final def encode(x: Int, y: Int): Long =
    (x & 0xffffffffL) | ((y & 0xffffffffL) << 32)

  // negation
  final def negate(a: Long): Long = encode(-x(a), -y(a))

  // addition
  final def add(a: Long, b: Long): Long = encode(x(a) + x(b), y(a) + y(b))

  // subtraction
  final def subtract(a: Long, b: Long): Long = encode(x(a) - x(b), y(a) - y(b))

}
 
/** Value class encoding a single precision 2D vector in a Long.
  * 
  * Based on Spire's FloatComplex class.
  */

class Float2D(val u: Long) extends AnyVal {
  override final def toString: String = "(%s, %s)" format (x, y)

  final def x: Float = FastFloat2D.x(u)
  final def y: Float = FastFloat2D.y(u)

  final def norm: Float = FastFloat2D.norm(u)
  final def magnitude: Float = FastFloat2D.norm(u)
  final def magnitude2: Float = FastFloat2D.norm2(u)

  final def angle: Float = FastFloat2D.angle(u)
  final def negate: Float2D = new Float2D(FastFloat2D.negate(u))

  final def +(b: Float2D) = new Float2D(FastFloat2D.add(u, b.u))
  final def -(b: Float2D) = new Float2D(FastFloat2D.subtract(u, b.u))

  final def :*(b: Float) = new Float2D(FastFloat2D.multiply(u, b))
  final def :/(b: Float) = new Float2D(FastFloat2D.divide(u, b))

  final def normalize = new Float2D(FastFloat2D.normalize(u))

  final def toInt: Int2D = Int2D(x.toInt, y.toInt)
}

object Float2D {
  import FastFloat2D.encode

  @inline final def apply(x: Float, y: Float): Float2D =
    new Float2D(encode(x, y))

  @inline final def apply(x: Double, y: Double): Float2D =
    new Float2D(encode(x.toFloat, y.toFloat))

  def polar(magnitude: Float, angle: Float) =
    new Float2D(FastFloat2D.polar(magnitude, angle))

  @inline final def zero = FastFloat2D.zero
  @inline final def ex = FastFloat2D.ex
  @inline final def ey = FastFloat2D.ey

  def random(r: Float = 1.0f, center: Float2D = Float2D(0, 0)) =
    Float2D(center.x + Random.nextFloat * r - r / 2,
      center.y + Random.nextFloat * r - r / 2)
}

/** FastFloat2D is an ugly, beautiful hack based on Spire's FastComplex
  * implementation.
  * 
  * Again, since we are overloading the meaning of Long, we cannot
  * ressort to a type-class based approach.
  */

object FastFloat2D {
  import java.lang.Math.{atan2, cos, sin, sqrt}

  // encode a float as some bits
  @inline final def bits(n: Float): Int = java.lang.Float.floatToRawIntBits(n)

  // decode some bits into a float
  @inline final def bits(n: Int): Float = java.lang.Float.intBitsToFloat(n)

  // get the x part of the 2D vector
  @inline final def x(d: Long): Float = bits((d & 0xffffffff).toInt)

  // get the y part of the 2D vector
  @inline final def y(d: Long): Float = bits((d >>> 32).toInt)

  @inline final def zero = new Float2D(0L)
  @inline final def ex = new Float2D(1065353216L)
  @inline final def ey = new Float2D(4575657221408423936L)

  // encode two floats representing a vector
  @inline final def encode(x: Float, y: Float): Long =
    (bits(x) & 0xffffffffL) | ((bits(y) & 0xffffffffL) << 32)

  // encode two floats representing a vector in polar form
  @inline final def polar(magnitude: Float, angle: Float): Long =
    encode(magnitude * cos(angle).toFloat, magnitude * sin(angle).toFloat)

  final def norm2(d: Long): Float = {
    val vx = x(d)
    val vy = y(d)
    vx * vx + vy * vy
  }

  // get the norm
  final def norm(d: Long): Float = java.lang.Math.sqrt(norm2(d)).toFloat

  // computes the normalized vector
  final def normalize(d: Long): Long = {
    val vx = x(d)
    val vy = y(d)
    val nrm = java.lang.Math.sqrt(vx * vx + vy * vy).toFloat
    encode(vx / nrm, vy / nrm)
  }

  // negation
  final def negate(a: Long): Long = encode(-x(a), -y(a))

  // addition
  final def add(a: Long, b: Long): Long = encode(x(a) + x(b), y(a) + y(b))

  // subtraction
  final def subtract(a: Long, b: Long): Long = encode(x(a) - x(b), y(a) - y(b))

  // multiplication
  final def multiply(a: Long, b: Float): Long = encode(x(a) * b, y(a) * b)

  // division
  final def divide(a: Long, b: Float): Long = encode(x(a) / b, y(a) / b)

  // get the angle/argument
  final def angle(d: Long): Float = atan2(y(d), x(d)).toFloat
}
