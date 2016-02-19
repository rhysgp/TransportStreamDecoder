package com.rhyssoft.tsdecoder

import java.io.InputStream

import scala.annotation.tailrec
import scala.util.{Failure, Success}

class TsInputStream(inputStream: InputStream, pid: Int) extends InputStream {

  private var currentPayload: Option[Array[Byte]] = None
  private var offset = 0

  override def read(): Int = {
    if (currentPayload.isEmpty) {
      currentPayload = Option(readPacket(pid).payload)
    }

    currentPayload.map{ payload =>
      val i = payload(offset).toInt
      if (offset >= payload.length)
        currentPayload = None
      else
        offset = offset + 1
      i
    }.getOrElse(-1)
  }

  @tailrec
  private def readPacket(pid: Int): TsPacket = {
    TsPacket.read(inputStream) match {
      case Success(tsPacket) =>
        if (tsPacket.packetIdentifier == pid) {
          tsPacket
        } else {
          readPacket(pid)
        }
      case Failure(t) =>
        throw t
    }

  }
}
