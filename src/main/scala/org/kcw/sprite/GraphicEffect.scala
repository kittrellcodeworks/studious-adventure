package org.kcw.sprite

import java.awt.Graphics2D

trait GraphicEffect {
  def paintEffect(g: Graphics2D): Unit
}
