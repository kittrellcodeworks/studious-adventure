package com.robowar

package object physics {
  private[physics] val tolerance: Double = 0.00000001D

  case class Dimension[T](w: T, h: T)
  case class Rect[T](x: T, y: T, w: T, h: T)
}
