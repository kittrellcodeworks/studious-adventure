package spritemodel

import java.awt.Graphics2D


trait GraphicEffect extends HasRemovalMarker {
  def paint(g: Graphics2D): Unit
}
