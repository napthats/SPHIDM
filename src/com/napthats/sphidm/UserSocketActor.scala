package com.napthats.sphidm

import scala.actors.Actor
import scala.actors.Actor._
import scala.util.control.Breaks._
import java.net.Socket
import java.net.SocketException
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.io.BufferedReader
import java.io.BufferedWriter


private case class _ClientMessage(message: String)
case class MessageToClient(message: String)
case class ClientMessage(message: String, clientId: ClientId)

case class DisconnectClient()

class UserSocketActor(clientSocket: Socket, myClientId: ClientId, messageDispatcher: Actor) extends Actor {
  private[this] val itsReader = new BufferedReader(new InputStreamReader(clientSocket.getInputStream(), UserSocketActor.CHARSET))
  private[this] val itsWriter = new BufferedWriter(new OutputStreamWriter(clientSocket.getOutputStream(), UserSocketActor.CHARSET))
  makeReceiveActor()
  
  private[this] def makeReceiveActor() {
    val mainActor = this
    actor {
      try {
        breakable {
          while(true) {
            val msg = itsReader.readLine();
            if (msg == null) break;
            mainActor ! _ClientMessage(msg)
          }
        }
      }
      catch {
        case e: SocketException =>
          //finished by mainActor
      }
    }
  }
  
  private[this] def send(msg: String) {
    if (msg.endsWith("\n")) {
	  itsWriter.write(msg,0,msg.length());
    } else {
	  itsWriter.write(msg,0,msg.length());
	  itsWriter.write('\n');
	}
	itsWriter.flush();
  }
  
  def act() {
    loop {
      react {
        case _ClientMessage(msg) =>
          messageDispatcher ! ClientMessage(msg, myClientId)
        case MessageToClient(msg) =>
          send(msg)
        case DisconnectClient() =>
          clientSocket.close()
          itsReader.close()
          itsWriter.close()
          exit()
        case _ =>
          error("Unknown message to UserSocketActor.")
      }
    }
  }
}

object UserSocketActor {
  private val CHARSET = "UTF-8"
  def apply(clientSocket: Socket, myClientId: ClientId, messageDispatcher: Actor) = new UserSocketActor(clientSocket, myClientId, messageDispatcher)
}