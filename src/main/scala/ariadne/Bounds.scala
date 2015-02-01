package at.ait.dme.forcelayout

/**
 * 2D bounds, plus some convenience methods.
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
case class Bounds(minX: Float, minY: Float, maxX: Float, maxY: Float) {
  
  lazy val width = maxX - minX
  
  lazy val height = maxY - minY
  
  lazy val center = Float2D((minX + maxX) / 2, (minY + maxY) / 2) 
  
  lazy val area = width * height
  
  def contains(pt: Float2D) = {
    if (pt.x < minX)
      false
    else if (pt.x > maxX)
      false
    else if (pt.y < minY)
      false
    else if (pt.y > maxY)
      false
    else
      true
  }
  
}
