package com.rhyssoft.tsdecoder

import java.io.{EOFException, InputStream}
import Utils._
import com.rhyssoft.tsdecoder.ScramblingControl.ScramblingControl

import scala.util.Try

case class TsPacket(
   syncByte: Int,
   errorIndicator: Boolean,
   payloadUnitStartIndicator: Boolean,
   transportPriority: Boolean,
   packetIdentifier: Int,
   scramblingControl: ScramblingControl,
   adaptationFieldFlag: Boolean,
   payloadFlag: Boolean,
   continuityCounter: Int,
   adaptationField: Option[AdaptationField],
   payload: Array[Byte]
)

case class AdaptationField(
  length: Int,
  discontinuityIndicator: Boolean,
  randomAccessIndicator: Boolean,
  elementaryStreamPriority: Boolean,
  pcrFlag: Boolean,
  opcrFlag: Boolean,
  splicingPointFlag: Boolean,
  transportPrivateDataFlag: Boolean,
  adaptationFieldExensionFlag: Boolean,
  pcr: Option[Array[Byte]],
  opcr: Option[Array[Byte]],
  spliceCountdown: Option[Byte],
  transportPrivateDataLength: Int,
  transportPrivateData: Option[Array[Byte]],
  adaptationExtension: Option[AdaptationExtensionField], // ???
  stuffingBytes: Array[Byte]
)

object TsPacket {

  def read(inputStream: InputStream): Try[TsPacket] = Try (readInternal(inputStream))

  private def readInternal(inputStream: InputStream): TsPacket = {

    var packetBytesRead = 0

    // ignore the first four bytes
    fillArray(inputStream, Array.ofDim[Byte](4))

    val arr = fillArray(inputStream, Array.ofDim[Byte](4))
    packetBytesRead = packetBytesRead + 4
    val streamHeader = fourBytesToInt(arr)

    val syncByte = (streamHeader & 0xff000000) >> 24
    val errorIndicator = (streamHeader & 0x800000) == 0x800000
    val payloadUnitStartIndicator = (streamHeader & 0x400000) == 0x400000
    val transportPriority = (streamHeader & 0x200000) == 0x200000
    val packetIdentifier = (streamHeader & 0x1fff00) >> 8
    val scramblingControl: ScramblingControl = (streamHeader & 0xc0) >> 3
    val adaptationFieldFlag = (streamHeader & 0x20) == 0x20
    val payloadFlag = (streamHeader & 0x10) == 0x10
    val continuityCounter = streamHeader & 0x0f

    val adaptationField = if (adaptationFieldFlag) {
      // read adaptation fields...
      val length = inputStream.read()
      var bytesRead = 0
      val flags = inputStream.read()

      val discontinuityIndicator = (flags & 0x80) == 0x80
      val randomAccessIndicator = (flags & 0x40) == 0x40
      val elementaryStreamPriorityIndicator = (flags & 0x20) == 0x20
      val pcrFlag = (flags & 0x10) == 0x10
      val opcrFlag = (flags & 0x08) == 0x08
      val splicingPointFlag = (flags & 0x04) == 0x04
      val transportPrivateDataFlog = (flags & 0x02) == 0x02
      val adaptationFieldExtensionFlag = (flags & 0x01) == 0x01
      bytesRead = bytesRead + 1

      val pcrData = if (pcrFlag) {
        bytesRead = bytesRead + 6
        Option(fillArray(inputStream, Array.ofDim[Byte](6)))
      } else None
      val opcrData = if (opcrFlag) {
        bytesRead = bytesRead + 6
        Option(fillArray(inputStream, Array.ofDim[Byte](6)))
      } else None
      val spliceCountdown = if (splicingPointFlag) {
        bytesRead = bytesRead + 1
        Option(fillArray(inputStream, Array.ofDim[Byte](1))(0))
      }  else None
      val privateDataLength = if (transportPrivateDataFlog) {
        bytesRead = bytesRead + 1
        inputStream.read()
      } else 0
      val privateData = if (transportPrivateDataFlog) {
        bytesRead = bytesRead + privateDataLength
        Option(fillArray(inputStream, Array.ofDim[Byte](privateDataLength)))
      } else None

      if (adaptationFieldExtensionFlag) {
        val extensionLength = inputStream.read()
        val data = fillArray(inputStream, Array.ofDim[Byte](extensionLength))
        val legalTimeWindow = (data(0) & 0x80) == 0x80
        val piecewiseRateFlag = (data(0) & 0x40) == 0x40
        val seamlessSpliceFlag = (data(0) & 0x20) == 0x20
        val reserved = data(0) & 0x1f
        var offset = 1

        val ltwFlagSet = if (legalTimeWindow) {
          val ltw = twoBytesToInt(data.slice(offset, offset + 2))
          offset = offset + 2
          Option(
            (ltw & 0x8000) == 0x8000,
            ltw & 0x7fff
          )
        } else None

        val piecewiseFlagSet = if (piecewiseRateFlag) {
          val piecewise = threeBytesToInt(data.slice(offset, offset + 3))
          offset = offset + 3
          Option(piecewise & 0x3fffff)
        } else None

        val seamlessSpliceFlagSet = if (seamlessSpliceFlag) {
          val seamlessSplice = fiveBytesToLong(data.slice(offset, offset + 5))
          Option(((seamlessSplice & 0xf000000000l) >> 36).toByte, seamlessSplice & 0x0efffefffel)
        } else None

        packetBytesRead = packetBytesRead + extensionLength + 1
      }

      val stuffingBytes = fillArray(inputStream, Array.ofDim[Byte](length - bytesRead))

      packetBytesRead = packetBytesRead + length + 1

      Option(AdaptationField(
        length,
        discontinuityIndicator,
        randomAccessIndicator,
        elementaryStreamPriorityIndicator,
        pcrFlag,
        opcrFlag,
        splicingPointFlag,
        transportPrivateDataFlog,
        adaptationFieldExtensionFlag,
        pcrData,
        opcrData,
        spliceCountdown,
        privateDataLength,
        privateData,
        None,
        stuffingBytes
      ))
    } else None

    val payload = fillArray(inputStream, Array.ofDim[Byte](188 - packetBytesRead))

    packetBytesRead = packetBytesRead + payload.length

    TsPacket(
      syncByte,
      errorIndicator,
      payloadUnitStartIndicator,
      transportPriority,
      packetIdentifier,
      scramblingControl,
      adaptationFieldFlag,
      payloadFlag,
      continuityCounter,
      adaptationField,
      payload
    )
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

case class AdaptationFieldsOptionalSubFields(hdr: AdaptationFieldHeader, optionalData: Array[Byte]) {
//  val pcrData =
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
      val bytesRead = inputStream.read(array, count, len - count)
      if (bytesRead == -1) throw new EOFException
      count = count + bytesRead
    }
    array
  }

  def twoBytesToInt(bytes: Array[Byte]): Int = (bytes(0) shl 8) or bytes(1)
  def threeBytesToInt(bytes: Array[Byte]): Int = (bytes(0) shl 16) or (bytes(1) shl 8) or bytes(0)
  def fourBytesToInt(bytes: Array[Byte]): Int = (bytes(0) shl 24) or (bytes(1) shl 16) or (bytes(2) shl 8) or bytes(3)
  def fiveBytesToLong(bytes: Array[Byte]): Int = (bytes(0) shl 32) or (bytes(1) shl 24) or (bytes(2) shl 16) or (bytes(3) shl 8) or bytes(4)

  case class Shiftable(i: Int) {
    def shl(n: Int) = i << n
    def or(s: Shiftable) = i | s.i
  }

  implicit def b2Shiftable(b: Byte): Shiftable = Shiftable(if (b < 0) 256 + b else b)
  implicit def i2shiftable(i: Int): Shiftable = Shiftable(i)
}



