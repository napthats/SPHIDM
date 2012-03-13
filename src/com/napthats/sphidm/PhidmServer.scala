package com.napthats.sphidm

import _root_.java.net.ServerSocket

 
object PhidmServer {
  val PORT = 20017
  Phidm.start()
  UserManager.start()
  PlayerCharacterManager.start()

  def main(args: Array[String]) {
    val serverSocket = new ServerSocket(PORT)
    while(true) {
      val clientSocket = serverSocket accept()
      UserManager ! AcceptNewUser(clientSocket)
    }
  }
}
