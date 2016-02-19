package com.rhyssoft.tsdecoder

import java.io._

import com.rhyssoft.io.IoUtils._

import scala.util.{Failure, Success}

object TsPacketApp extends App {

  val startTime = System.currentTimeMillis()
  val inFile = args(0)
  val outFile = args(1)
  val desc = args.length > 2 && args(2) == "desc"

  process()

  val endTime = System.currentTimeMillis()

  println(s"Processing took ${endTime - startTime} ms")

  def process(): Unit = {
    autoClose(new BufferedInputStream(new FileInputStream(inFile))) { inStream =>
      if (desc) {
        autoClose(new BufferedWriter(new FileWriter(outFile))) { writer =>
          var done = false
          while (!done) {
            TsPacket.read(inStream) match {
              case Success(packet) =>
                writer.write(packetToString(packet))
              case Failure(t: EOFException) =>
                done = true
              case Failure(t) =>
                throw t
            }
          }
        }
      } else {
        autoClose(new BufferedOutputStream(new FileOutputStream(outFile))) { outStream =>
          var done = false
          while (!done) {


            TsPacket.read(inStream) match {
              case Success(packet) =>
                outStream.write(packet.payload)
              case Failure(t: EOFException) =>
                done = true
              case Failure (t) =>
                throw t
            }
          }
        }
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
    println("PID Type               : %s".format(tsPacket.pidType.toString))
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
