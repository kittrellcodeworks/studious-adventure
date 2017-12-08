package org.kcw.sprite

import java.awt.Graphics2D

trait GraphicEffect {
  def paint(g: Graphics2D): Unit
}
