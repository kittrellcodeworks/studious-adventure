package org.kcw.sampleapp

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{JMenuItem, KeyStroke}

object MenuItem {

  // example: MenuItem("Save As...", KeyEvent.VK_S, java.awt.event.InputEvent.SHIFT_MASK)
  def apply(name: String, key: Int, modifiers: Int*): JMenuItem = {
    val menu = new JMenuItem(name)
    menu.setAccelerator(KeyStroke.getKeyStroke(key, modifiers.foldLeft(MenuShortcutKeyMask)(_ | _)))
    menu
  }

  // example: MenuItem("Save As...", _ => doSaveAs(), KeyEvent.VK_S, java.awt.event.InputEvent.SHIFT_MASK)
  def apply(name: String, f: ActionEvent ⇒ Unit, key: Int, modifiers: Int*): JMenuItem = {
    val menu = apply(name, key, modifiers: _*)
    menu.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = f(e)
    })
    menu
  }
}
