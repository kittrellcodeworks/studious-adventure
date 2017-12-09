package org.kcw.sampleapp

import java.awt.BorderLayout
import java.awt.event.{WindowAdapter, WindowEvent}
import javax.swing.{JFrame, UIManager, WindowConstants}

import scala.compat.Platform.EOL
import scala.util.control.NonFatal

object SampleApp extends App {

  private class ExceptionHandler extends Thread.UncaughtExceptionHandler
  {
    override def uncaughtException(thread: Thread, thrown: Throwable): Unit = {
      val msg = Option(thrown.getMessage).map(": " + _).getOrElse("")
      val trace = Option(thrown.getStackTrace).map(_.mkString(EOL, EOL, EOL)).getOrElse("")
      System.err.println("Uncaught " + thrown.getClass.getSimpleName + msg + trace)
    }
  }

  Thread.setDefaultUncaughtExceptionHandler(new ExceptionHandler)

  try {
    // This doesn't seem to work.
    System.setProperty("apple.laf.useScreenMenuBar", "true")

    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())

    val frame = new JFrame(AppName)
    val arena = new ArenaCanvas

    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.addWindowListener(new WindowAdapter {
      override def windowClosed(e: WindowEvent): Unit = exit()
    })

    frame.getContentPane().add(arena, BorderLayout.CENTER)
    frame.getInsets
    frame.pack()

    frame.setVisible(true)

  } catch {
    case NonFatal(e) ⇒ e.printStackTrace; exit()
  }

  def exit(): Unit = {
    System.exit(0)
  }
}
