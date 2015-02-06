package com.faacets.ariadne

object Utils {
  @inline final def fastInverseSquareRoot(x: Float): Float = {
    val xHalf: Float = 0.5f * x
    var temp: Int = java.lang.Float.floatToRawIntBits(x)
    temp = 0x5F3759DF - (temp >> 1)
    var newX: Float  = java.lang.Float.intBitsToFloat(temp)
    newX *= (1.5F - xHalf * newX * newX)
    newX
  }
  @inline final def fastSquareRoot(x: Float): Float =
    fastInverseSquareRoot(x) * x
  @inline final def fastInverseSquareRoot(x: Double): Double = {
    var xhalf: Double = 0.5d*x
    var i: Long = java.lang.Double.doubleToLongBits(x);
    i = 0x5fe6ec85e7de30daL - (i>>1)
    var newX = java.lang.Double.longBitsToDouble(i)
    newX *= (1.5d - xhalf*newX*newX)
    newX *= (1.5d - xhalf*newX*newX)
    newX *= (1.5d - xhalf*newX*newX)
    newX *= (1.5d - xhalf*newX*newX)
    newX
  }
  @inline final def fastSquareRoot(x: Double): Double =
    fastInverseSquareRoot(x) * x        
}
