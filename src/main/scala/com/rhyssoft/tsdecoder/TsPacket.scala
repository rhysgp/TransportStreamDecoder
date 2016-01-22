package com.rhyssoft.tsdecoder

import java.io.InputStream
import Utils._
import ScramblingControl._

case class TsPacket(
   syncByte: Int,
   errorIndicator: Boolean,
   payloadUnitStartIndicator: Boolean,
   transportPriority: Boolean,
   packetIdentifier: Int,
   scramblingControl: ScramblingControl,
   adaptationFieldFlag: Boolean,
   payloadFlag: Boolean,
   continuityCounter: Boolean
)

object TsPacket {
  def read(inputStream: InputStream): TsPacket = {
    val streamHeader = fourBytesToInt(fillArray(inputStream, Array.ofDim[Byte](4)))

    val syncByte = (streamHeader & 0xff000000) >> 24
    val errorIndicator = (streamHeader & 0x800000) == 0x800000
    val payloadUnitStartIndicator = (streamHeader & 0x400000) == 0x400000
    val transportPriority = (streamHeader & 0x200000) == 0x200000
    val packetIdentifier = (streamHeader & 0x1fff00) >> 8
    val scramblingControl: ScramblingControl = (streamHeader & 0xc0) >> 3
    val adaptationFieldFlag = (streamHeader & 0x20) == 0x20
    val payloadFlag = (streamHeader & 0x10) == 0x10
    val continuityCounter = (streamHeader & 0x0f) == 0x0f

    if (adaptationFieldFlag) {
      // read adaptation fields...
      val length = inputStream.read()
      val data = fillArray(inputStream, Array.ofDim[Byte](length), 1)
      data.update(0, length.asInstanceOf[Byte])

      val discontinuityIndicator = (data(1) & 0x80) == 0x80
      val randomAccessIndicator = (data(1) & 0x40) == 0x40
      val elementaryStreamPriorityIndicator = (data(1) & 0x20) == 0x20
      val pcrFlag = (data(1) & 0x10) == 0x10
      val opcrFlag = (data(1) & 0x08) == 0x08
      val splicingPointFlag = (data(1) & 0x04) == 0x04
      val transportPrivateDataFlog = (data(1) & 0x02) == 0x02
      val adaptationFieldExtensionFlog = (data(1) & 0x01) == 0x01


    }


  }
}

case class AdaptationFieldHeader(data: Int) {
  def length = (data & 0x0000ff00) >> 8
  def discontinuityIndicator = (data & 0x80) == 0x80
  def randomAccessIndicator = (data & 0x40) == 0x40
  def elementaryStreamPriorityIndicator = (data & 0x20) == 0x20
  def pcrFlag = (data & 0x10) == 0x10
  def opcrFlag = (data & 0x08) == 0x08
  def splicingPointFlag = (data & 0x04) == 0x04
  def transportPrivateDataFlog = (data & 0x02) == 0x02
  def adaptationFieldExtensionFlog = (data & 0x01) == 0x01
}

case class AdaptationFieldsOptionalSubFields(hdr: AdaptationFieldHeader, optionalData: Byte[Array]) {
  val pcrData =
}

object AdaptationFieldsOptionalSubFields {
  def read(hdr: AdaptationFieldHeader, inputStream: InputStream) = {



    AdaptationFieldsOptionalSubFields
  }
}

case class AdaptationExtensionField(
  hdr: AdaptationExtensionFieldHeader,
  ltwFlagSet: Option[LtwFlagSet]
)

case class AdaptationExtensionFieldHeader(extFldHdr: Int) {
  def headerLength = (extFldHdr & 0xff00) >> 8
  def ltw = (extFldHdr & 0x0040) == 0x0040
  def seamlessSpliceFlag = (extFldHdr & 0x0020) == 0x0020
  def reserved = extFldHdr & 0x001f
}

case class LtwFlagSet(ltwFlagSet: Int) {
  def ltwValid = (ltwFlagSet & 0x8000) == 0x8000
  def ltwOffset = ltwFlagSet & 0x7fff
}

case class PiecewiseFlagSet(flagSet: Int) {
  def reserved = (flagSet & 0xc00000) >> 26
  def rate = flagSet & 0x3fffff
}

case class SeamlessSpliceFlagSet(flagSet: Long) {
  def spliceType = (flagSet & 0xf000000000l) >> 36
  def nextAccessUnit = flagSet & 0x0efffefffel
}

//object AdaptationExtensionField {
//
//  def read(inputStream: InputStream): AdaptationExtensionField = {
//    val headerLength = inputStream.read()
//    val packetData = fillArray(inputStream, Array.ofDim[Byte](headerLength), 1)
//    val extFldHeader = twoBytesToInt(packetData)
//
//    if (extFldHeader.)
//
//
//    AdaptationExtensionField(extFldHeader)
//  }
//
//}

object ScramblingControl extends Enumeration {
  type ScramblingControl = Value
  val notScrambled, reserved, scrambledEven, scrambledOdd = Value

  implicit def intToScramblingControl(i: Int): ScramblingControl =
    if (i == 1)
      reserved
    else if (i == 2) {
      scrambledEven
    } else if (i == 3) {
      scrambledOdd
    } else {
      notScrambled
    }
}

object Utils {

  def fillArray(inputStream: InputStream, array: Array[Byte], initialCount: Int = 0): Array[Byte] = {
    var count = initialCount
    val len = array.length
    while (count < len) {
      count = inputStream.read(array, count, len - count)
    }
    array
  }

  def fourBytesToInt(bytes: Array[Byte]): Int = (bytes(0) shl 24) or (bytes(1) shl 16) or (bytes(2) shl 8) or bytes(3)
  def twoBytesToInt(bytes: Array[Byte]): Int = (bytes(0) shl 8) or bytes(1)
  def fiveBytesToLong(bytes: Array[Byte]): Int = (bytes(0) shl 32) or (bytes(1) shl 24) or (bytes(2) shl 16) or (bytes(3) shl 8) or bytes(4)

  case class Shiftable(i: Int) {
    def shl(n: Int) = i << n
    def or(s: Shiftable) = i | s.i
  }

  implicit def b2Shiftable(b: Byte): Shiftable = Shiftable(if (b < 0) 256 + b else b)
  implicit def i2shiftable(i: Int): Shiftable = Shiftable(i)
}



