package com.napthats.sphidm

import scala.actors.Actor
import scala.actors.Actor._
import java.net.Socket


case class AcceptNewUser(clientSocket: Socket)
case class ClientId(id: Int)

sealed abstract class UserState
case class Initial extends UserState
case class Opened(pcId: PlayerCharacterId) extends UserState

private case class User(myClientId: ClientId, mySocketActor: UserSocketActor, myState: UserState)

case class ExitPc(pcId: PlayerCharacterId)

object UserManager extends Actor {
  private[this] val userContainer = scala.collection.mutable.Map[ClientId, User]()
  private[this] val pcId2ClientId = scala.collection.mutable.Map[PlayerCharacterId, ClientId]()
  private[this] var nextClientIdNum = 0
  
  def act() {
    loop {
      react {
        //from PlayerCharacterManager
        case x @ MessageToUser(pcId) =>
          val User(_, socketActor, _) = userContainer(pcId2ClientId(pcId))
          x match {
            case PlainMessageToUser(_, msg) =>
              socketActor ! MessageToClient(msg)
            case CommandToUser(_, command) =>
              command match {
                case Close() =>
                  val clientId = pcId2ClientId(pcId)
                  socketActor ! DisconnectClient()
                  userContainer.remove(clientId)
                  pcId2ClientId.remove(pcId)
                  PlayerCharacterManager ! LogoutPlayerCharacter(pcId)
              }
          }

        case AcceptNewUser(clientSocket) =>
          val newClientId = getNextClientId
          val userSocketActor = UserSocketActor(clientSocket, newClientId, this)
          userSocketActor.start()
          userContainer(newClientId) = User(newClientId, userSocketActor, Initial())

        case x @ LoginPlayerCharacterResult(clientId) =>
          val User(_, socketActor, _) = userContainer(clientId)
          x match {            
            case LoginSucceeded(_, pcId) =>
              userContainer(clientId) match {
                case User(_, _, Initial()) =>
                  userContainer(clientId) = User(clientId, socketActor, Opened(pcId))
                  pcId2ClientId(pcId) = clientId
                case User(_, _, Opened(_)) =>
                  error("Assertion error: User tried to login already logined.")
              }
            case NoSuchUser(_) =>
              socketActor ! MessageToClient("No such user.")
              socketActor ! DisconnectClient()
              userContainer.remove(clientId)
            case UserAlreadyLogined(_, pcId) =>
              socketActor ! MessageToClient("User already connected.")
              socketActor ! DisconnectClient()
              userContainer.remove(clientId)
          }
          
        case ClientMessage(msg, clientId) =>
          PhidmProtocolParser.protocolToCommand(msg) match {
            case Open(id) =>
              userContainer(clientId) match {
                case User(_, _, Opened(_)) =>
                case User(_, mySocketActor, Initial()) =>
                  PlayerCharacterManager ! LoginPlayerCharacter(clientId, PhircId(id))
              }
            case x =>
              userContainer(clientId) match {
                case User(_, _, Opened(pcId)) =>
                  PlayerCharacterManager ! UserMessage(clientId, pcId, x)
                case User(_, _, Initial()) =>
              }
          }
          
        case x => 
          error("Unknown message to UserManager: " + x.toString())
      }
    }
  }
  
  private[this] def getNextClientId: ClientId = {
    val currentClientId = nextClientIdNum
    nextClientIdNum += 1
    ClientId(currentClientId)
  }
}

