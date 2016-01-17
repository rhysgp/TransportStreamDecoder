package com.rhyssoft.tsdecoder

case class TsPacket(packetData: Array[Byte]) {

  import Utils._

  lazy val streamHeader = packetData.slice(0, 4)

//  def syncByte =

  def errorIndicator: Boolean = (packetData(4) & 0x00000001) == 0x00000001

}


object Utils {

  def bytesToInt(bytes: Array[Byte]): Int = (bytes(0) shl 24) or (bytes(1) shl 16) or (bytes(2) shl 8) or bytes(3)

  case class Shiftable(i: Int) {
    def shl(n: Int) = i << n

    def or(s: Shiftable) = i | s.i
  }

  implicit def b2Shiftable(b: Byte): Shiftable =
    if (b < 0)
      Shiftable(256 + b)
    else
      Shiftable(b)

  implicit def i2shiftable(i: Int): Shiftable = Shiftable(i)
}



