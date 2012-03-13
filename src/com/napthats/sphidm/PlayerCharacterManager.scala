package com.napthats.sphidm

import scala.actors.Actor
import scala.actors.Actor._


abstract case class MessageToUser(pcId: PlayerCharacterId)
case class PlainMessageToUser(_pcId: PlayerCharacterId, msg: String) extends MessageToUser(_pcId)
case class CommandToUser(_pcId: PlayerCharacterId, protocol: PhiServerToClientProtocol) extends MessageToUser(_pcId)

case class PhircId(id: String)
case class PlayerCharacterId(pcId: Int)

case class UserMessage(clientId: ClientId, pcId: PlayerCharacterId, protocolMessage: PhiProtocol)

case class PlayerCharacterStatus(hp: Int, mhp: Int, mp: Int, mmp: Int, karma: Int, money: Int, flevel: Int, wlevel: Int, mlevel: Int, clevel: Int, exp: Int, x: Int, y: Int, dir: Int)

abstract case class PlayerCharacterState
case class Logout extends PlayerCharacterState
case class Login(pcId: PlayerCharacterId) extends PlayerCharacterState
case class PlayerCharacterData(phircIdString: String, name: String, pcStatus: PlayerCharacterStatus)

case class LoginPlayerCharacter(clientId: ClientId, phircId: PhircId)
case class LogoutPlayerCharacter(pcId: PlayerCharacterId)

abstract case class LoginPlayerCharacterResult(clientId: ClientId)
case class LoginSucceeded(_clientId: ClientId, pcId: PlayerCharacterId) extends LoginPlayerCharacterResult(_clientId)
case class NoSuchUser(_clientId: ClientId) extends LoginPlayerCharacterResult(_clientId)
case class UserAlreadyLogined(_clientId: ClientId, pcId: PlayerCharacterId) extends LoginPlayerCharacterResult(_clientId)


object PlayerCharacterManager extends Actor{
  private[this] val playerCharacterDataContainer = scala.collection.mutable.Map[PhircId, PlayerCharacterData]()
  private[this] val loginedPlayerCharacterContainer = scala.collection.mutable.Map[PlayerCharacterId, PlayerCharacter]()
  private[this] val playerCharacterStateContainer = scala.collection.mutable.Map[PhircId, PlayerCharacterState]()
  private[this] val pcId2phircId = scala.collection.mutable.Map[PlayerCharacterId, PhircId]()
  private[this] var nextPlayerCharacterIdNum = 0
  
  playerCharacterDataContainer(PhircId("test")) = PlayerCharacterData("test", "test chara", PlayerCharacterStatus(1000,2000,500,800,10,1000,1000,1000,1000,1000,100,0,0,0))
  playerCharacterStateContainer(PhircId("test")) = Logout()
  
  def act() {
    loop {
      react {
        case x @ MessageToUser(_) =>
          UserManager ! x
        
        case LoginPlayerCharacter(clientId, phircId) =>
          playerCharacterStateContainer.get(phircId) match {
            case Some(Login(pcId)) =>
              UserManager ! UserAlreadyLogined(clientId, pcId)
            case Some(Logout()) =>
              val newPcId =  makeNewPlayerCharacter(playerCharacterDataContainer(phircId))
              playerCharacterStateContainer(phircId) = Login(newPcId)
              pcId2phircId(newPcId) = phircId
              UserManager ! LoginSucceeded(clientId, newPcId)
            case None => 
              UserManager ! NoSuchUser(clientId)
          }
          
        case x @ LogoutPlayerCharacter(pcId) =>
          val phircId = pcId2phircId(pcId)
          pcId2phircId.remove(pcId)
          playerCharacterStateContainer(phircId) = Logout()
          val pc = loginedPlayerCharacterContainer(pcId)
          loginedPlayerCharacterContainer.remove(pcId)
          pc ! x
          
        case UserMessage(clientId, pcId, msg) =>
          loginedPlayerCharacterContainer(pcId) ! UserMessage(clientId, pcId, msg)
          
        case x =>
          error("Unknown message to PlayerCharacterManager: " + x.toString())
      }
    }
  }
  
  private[this] def makeNewPlayerCharacter(pcData: PlayerCharacterData): PlayerCharacterId = {
    val currentPlayerCharacterId = PlayerCharacterId(nextPlayerCharacterIdNum)
    val pc = PlayerCharacter(pcData, currentPlayerCharacterId)
    pc.start()
    nextPlayerCharacterIdNum += 1
    loginedPlayerCharacterContainer(currentPlayerCharacterId) = pc 
    currentPlayerCharacterId
  }
}
