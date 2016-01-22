package com.rhyssoft.com.tsdecoder

import com.rhyssoft.tsdecoder.Utils
import org.specs2.mutable.Specification

class TsPacketSpec extends Specification{

  "bytesToInt" should {
    "produce the correct Int with positive integer in low byte" in {
      Utils.fourBytesToInt(Array[Byte](0, 0, 0, 1)) mustEqual 0x00000001
    }
    "produce the correct Int with positive integer in second to low byte" in {
      Utils.fourBytesToInt(Array[Byte](0, 0, 1, 0)) mustEqual 0x00000100
    }
    "produce the correct Int with positive integer in second to high byte" in {
      Utils.fourBytesToInt(Array[Byte](0, 1, 0, 0)) mustEqual 0x00010000
    }
    "produce the correct Int with positive integer in high byte" in {
      Utils.fourBytesToInt(Array[Byte](1, 0, 0, 0)) mustEqual 0x01000000
    }




    "produce the correct Int with negative integer in low byte" in {
      Utils.fourBytesToInt(Array[Byte](0, 0, 0, -1)) mustEqual 0x000000ff
    }
    "produce the correct Int with negative integer in second to low byte" in {
      Utils.fourBytesToInt(Array[Byte](0, 0, -1, 0)) mustEqual 0x0000ff00
    }
    "produce the correct Int with negative integer in second to high byte" in {
      Utils.fourBytesToInt(Array[Byte](0, -1, 0, 0)) mustEqual 0x00ff0000
    }
    "produce the correct Int with negative integer in high byte" in {
      Utils.fourBytesToInt(Array[Byte](-1, 0, 0, 0)) mustEqual 0xff000000
    }
  }

}
