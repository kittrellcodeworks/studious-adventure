package com.robowar
package application

import java.awt.event.{WindowAdapter, WindowEvent, WindowListener}

import com.typesafe.config.ConfigFactory
import java.awt.{BorderLayout, Canvas, Toolkit}
import javax.swing.{JFrame, JLabel, UIManager, WindowConstants}

import akka.actor._
import scala.util.control.NonFatal
import scala.compat.Platform.EOL

object Robowar extends App {

  private class ExceptionHandler extends Thread.UncaughtExceptionHandler
  {
    override def uncaughtException(thread: Thread, thrown: Throwable): Unit = {
      val msg = Option(thrown.getMessage).map(": " + _).getOrElse("")
      val trace = Option(thrown.getStackTrace).map(_.mkString(EOL, EOL, EOL)).getOrElse("")
      System.err.println("Uncaught " + thrown.getClass.getSimpleName + msg + trace)
    }
  }

  private class Terminator(app: ActorRef) extends Actor with ActorLogging {
    context watch app
    def receive = {
      case Terminated(_) ⇒
        context.system.terminate()
        System.exit(0)
    }
  }

  Thread.setDefaultUncaughtExceptionHandler(new ExceptionHandler)

  val system = ActorSystem("Main")

  // Lazy in order to catch initialization exceptions in the `try`, below, but still be accessible
  private lazy val app = system.actorOf(Props(classOf[actors.Application]), "app")
  private lazy val terminator = system.actorOf(Props(classOf[Terminator], app), "app-terminator")

  try {

    terminator // initialize the actors

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
    case NonFatal(e) ⇒ system.terminate(); throw e
  }

  def exit(): Unit = {
    system.stop(app)
  }
}
