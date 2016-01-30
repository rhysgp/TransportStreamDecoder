package com.rhyssoft.tsdecoder

import java.io.{File, FileInputStream, FileWriter}

import com.rhyssoft.io.IoUtils._

object TsPacketApp extends App {

  autoClose(new FileWriter(new File("output.log"))) { writer =>
    autoClose(new FileInputStream(args(0))){ stream =>
      while (true) {
        writer.write(packetToString(TsPacket.read(stream)))
      }
    }
  }

  def packetToString(tsPacket: TsPacket): String = {

    val sb = new StringBuilder

    def println(s: String = "") = sb.append(s).append("\n")

    println()
    println("-- transport packet header --")
    println("sync byte              : 0x%02x".format(tsPacket.syncByte))
    println("error indicator        : %b".format(tsPacket.errorIndicator))
    println("payload unit start     : %b".format(tsPacket.payloadUnitStartIndicator))
    println("transport priority     : %b".format(tsPacket.payloadUnitStartIndicator))
    println("PID                    : %d".format(tsPacket.packetIdentifier))
    println("scrambling control     : %s".format(tsPacket.scramblingControl.toString))
    println("adaptation fld flag    : %b".format(tsPacket.adaptationFieldFlag))
    println("payload fld flag       : %b".format(tsPacket.payloadFlag))
    println("continuity counter     : 0x%02x".format(tsPacket.continuityCounter))

    tsPacket.adaptationField.foreach{ adaptationField =>
      println()
      println("-- adaptation field --")
      println("field length           : %d".format(adaptationField.length))
      println("discontinuity indicator: %b".format(adaptationField.discontinuityIndicator))
      println("random access indicator: %b".format(adaptationField.randomAccessIndicator))
      println("elementa stream pri ind: %b".format(adaptationField.elementaryStreamPriority))
      println("PCR flag               : %b".format(adaptationField.pcrFlag))
      println("OPCR flag              : %b".format(adaptationField.opcrFlag))
      println("splicing point flag    : %b".format(adaptationField.splicingPointFlag))
      println("trans private data flag: %b".format(adaptationField.transportPrivateDataFlag))
      println("adapt fld extension flg: %b".format(adaptationField.adaptationFieldExensionFlag))
      adaptationField.pcr.foreach { pcr =>
        println("PCR                    : %s".format(outputLine(pcr, pcr.length, pcr.length)))
      }
      println("stuffing bytes (length): %d".format(adaptationField.stuffingBytes.length))
    }
    println("payload bytes (length) : %d".format(tsPacket.payload.length))
    println("-----------------------------------")
    println()

    sb.toString()
  }

  def outputLine(buf: Array[Byte], count: Int, minBuffSize: Int = 16): String = {
    val strBuilder = new StringBuilder

    for (i <- 0 until count) {
      strBuilder.append("%02x ".format(buf(i)))
    }

    for (i <- count until minBuffSize) {
      strBuilder.append("   ")
    }

    for (i <- 0 until count) {
      val b = buf(i)
      if (b >= 32 && b < 128) {
        strBuilder.append(b.asInstanceOf[Char])
      } else {
        strBuilder.append(".")
      }
    }

    strBuilder.toString()
  }

}
