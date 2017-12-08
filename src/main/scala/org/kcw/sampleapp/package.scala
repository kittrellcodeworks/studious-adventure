package org.kcw

import java.awt.Toolkit
import java.awt.event.{MouseAdapter, MouseEvent}

package object sampleapp {

  val AppName = "Robowar"
  val MenuShortcutKeyMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()

  case class PopupMenuMouseAdapter(showPopup: MouseEvent ⇒ Unit) extends MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = evaluatePopup(e)

    override def mouseReleased(e: MouseEvent): Unit = evaluatePopup(e)

    private def evaluatePopup(e: MouseEvent): Unit = if (e.isPopupTrigger) showPopup(e)
  }

}
