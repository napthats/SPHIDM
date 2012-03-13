package com.napthats.sphidm

import scala.actors.Actor
import scala.actors.Actor._

class PlayerCharacter(pcData: PlayerCharacterData, myPcId: PlayerCharacterId) extends Actor {

  def act() {
    loop {
      react {
        case UserMessage(_, _, Exit()) =>
          PlayerCharacterManager ! CommandToUser(myPcId, Close())
          
        case UserMessage(_, _, msg) =>
          PlayerCharacterManager ! PlainMessageToUser(myPcId, "Your message: " + msg)
          
        case LogoutPlayerCharacter(_) =>
          exit()
          
        case x =>
          error("Unknown message to PlayerCharacter: " + x.toString())
      }
    } 
  }
}

object PlayerCharacter {
  def apply(pcData: PlayerCharacterData, pcId: PlayerCharacterId) = new PlayerCharacter(pcData, pcId)
}
