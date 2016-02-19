package com.rhyssoft.com.tsdecoder

import java.io._

import com.rhyssoft.io.IoUtils._
import com.rhyssoft.tsdecoder.{TsPacket, TsInputStream}

import scala.util.{Failure, Success}


object CheckPids extends App {

  val inFile = new File("/Users/rhys/Documents/BBC Proms 2015/BBC Proms 2015_20150903_2213.ts" )

  autoClose(new BufferedInputStream(new FileInputStream(inFile))) { inStream =>

    var pidMap = Map[Int,Int]()
//    var pids = Set[Int]()

    var done = false
    while (!done) {
      TsPacket.read(inStream) match {
        case Success(packet) =>
          val count = pidMap.getOrElse(packet.packetIdentifier, 0)
          pidMap = pidMap + (packet.packetIdentifier -> (count + 1))
//          pids = pids + packet.packetIdentifier

        case Failure(t: EOFException) =>
          done = true
        case Failure (t) =>
          throw t
      }
    }

    println(pidMap.toSeq.sortBy(_._2).map{ case(k,v) => f"$k -> $v" }.mkString("\n"))
  }


}
