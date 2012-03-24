package com.napthats.sphidm


sealed abstract class PhiProtocol

case class PhiServerToClientProtocol extends PhiProtocol

case class Close extends PhiServerToClientProtocol
case class Tmp extends PhiServerToClientProtocol

case class PhiClientToServerProtocol extends PhiProtocol

case class NormalMessage(msg: String) extends PhiClientToServerProtocol

//client protocol starting with #
case class PhiClientSpecialProtocol extends PhiClientToServerProtocol
case class Open(id: String) extends PhiClientSpecialProtocol
case class EndLag extends PhiClientSpecialProtocol
case class Reserve(id: String) extends PhiClientSpecialProtocol
case class NoSrv extends PhiClientSpecialProtocol
case class Trans(id: String) extends PhiClientSpecialProtocol
case class ChSrvNo extends PhiClientSpecialProtocol
case class ChSrvOk extends PhiClientSpecialProtocol
case class CodeEuc extends PhiClientSpecialProtocol
case class CodeUtf extends PhiClientSpecialProtocol
case class CodeSjis extends PhiClientSpecialProtocol
case class VersionClient(version: String) extends PhiClientSpecialProtocol
case class LeaveWin extends PhiClientSpecialProtocol
case class EnterWin extends PhiClientSpecialProtocol
case class Map extends PhiClientSpecialProtocol
case class MapIv(iv: Int) extends PhiClientSpecialProtocol
case class Status extends PhiClientSpecialProtocol
case class StatusIv(iv: Int) extends PhiClientSpecialProtocol
case class Ulist extends PhiClientSpecialProtocol
case class SpecialPriv(name: String, message: String) extends PhiClientSpecialProtocol
case class X extends PhiClientSpecialProtocol
case class UnknownSpecialProtocol(msg: String) extends PhiClientSpecialProtocol

//client plaintext command
case class PhiCommand extends PhiClientToServerProtocol
case class Move(d: Direction) extends PhiCommand
case class Turn(d: Direction) extends PhiCommand
case class Read extends PhiCommand
case class Board extends PhiCommand
case class Use extends PhiCommand
case class Erase extends PhiCommand
case class FloorItem extends PhiCommand
case class Guard extends PhiCommand
case class Check extends PhiCommand
case class Look extends PhiCommand
case class Pay(amount: Int) extends PhiCommand
case class Equip extends PhiCommand
case class Spells extends PhiCommand
case class Write extends PhiCommand
case class Sort extends PhiCommand
case class Unequip extends PhiCommand
case class Put extends PhiCommand
//case class Y extends PhiCommand
case class Get extends PhiCommand
case class Hit extends PhiCommand
case class Cast extends PhiCommand
case class Exit extends PhiCommand
case class Chat extends PhiCommand
case class Title extends PhiCommand
case class Priv(name: String, message: String) extends PhiCommand
case class Action extends PhiCommand
case class Version extends PhiCommand
case class See extends PhiCommand
case class ExSwitch(key: String, value: String) extends PhiCommand
case class ExMap(key: String, value: String) extends PhiCommand

sealed abstract class Direction
sealed abstract case class RelativeDirection extends Direction
case class Forward extends RelativeDirection
case class Backward extends RelativeDirection
case class Right extends RelativeDirection
case class Left extends RelativeDirection
sealed abstract case class AbsoluteDirection extends Direction
case class North extends AbsoluteDirection
case class South extends AbsoluteDirection
case class East extends AbsoluteDirection
case class West extends AbsoluteDirection


//message for making protocol
sealed abstract class MessageToMakeProtocolString
case class AroundMapData(dir: AbsoluteDirection, time: Int, ) extends MessageToMakeProtocolString

object PhidmProtocolParser {
  def protocolToCommand(msg:String): PhiProtocol = {
    val (protocolType, _remainingString) = if(msg.indexOf(' ') >= 0) msg.splitAt(msg.indexOf(' ')) else msg.splitAt(msg.length)
    val remainingString = _remainingString.drop(1)
    protocolType match {
      case "open" =>
        Open(remainingString)
      case "exit" =>
        Exit()
      case _ =>
        NormalMessage(msg)
    }
  }
  
  def makeProtocolString(x: MessageToMakeProtocolString): String = {
    x match {
      
    }
  }
}
