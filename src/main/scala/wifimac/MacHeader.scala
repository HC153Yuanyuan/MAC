package wifimac

import spinal.core.{RegInit, _}
import spinal.lib._

object  PackFormat extends SpinalEnum {
  val format_11b:Bits = B"0000"
  val format_11g:Bits = B"0001"
  val format_11n:Bits = B"0010"
  val format_vht_su:Bits = B"0011"
  val format_vht_mu:Bits = B"0100"
  val format_he_su:Bits = B"0101"
  val format_he_ersu:Bits = B"0110"
  val format_he_mu:Bits = B"0111"
  val format_he_tb:Bits = B"1000"
}

object FrameCtrl {
  val PV = Bits(2 bits)
  val TYPE = Bits(2 bits)
  val SUBTYPE = Bits(4 bits)
  val TODS = Bits(1 bit)
  val FROMDS = Bits(1 bit)
  val MOREFRAG = Bits(1 bit)
  val RETRY = Bits(1 bit)
  val PM = Bits(1 bit)
  val MOREDATA = Bits(1 bit)
  val PROTECTED = Bits(1 bit)
  val HTC = Bits(1 bit)

  def packFC():Bits = {
    B(PV,TYPE,SUBTYPE,TODS,FROMDS,MOREFRAG,RETRY,PM,MOREDATA,PROTECTED,HTC)
  }

  def unpackFC(raw:Bits) = {
    (PV,TYPE,SUBTYPE,TODS,FROMDS,MOREFRAG,RETRY,PM,MOREDATA,PROTECTED,HTC) := raw
  }
}


object MacHeader {
  val frameControl = FrameCtrl
  val duration_id = Bits(16 bits)
  val addr1 = Bits(48 bits)
  val addr2 = Bits(48 bits)
  val addr3 = Bits(48 bits)
  val seq_ctrl = Bits(16 bits)
  val addr4 = Bits(48 bits)
  val qos_ctrl = Bits(16 bits)
  val ht_ctrl = Bits(16 bits)

  def getRA():Bits = {
    addr1
  }
  def getTA():Bits = {
    addr2
  }
  def getBSSID():Bits = {
    val bssid = Bits(48 bits)
    switch(B(frameControl.TODS,frameControl.FROMDS).asUInt) {
      is (0) { bssid := addr3 }
      is (1) { bssid := addr2 }
      is (2) { bssid := addr1 }
      is (3) { bssid := 0 }
    }
    bssid
  }

  def getMacHeaderLen():Bits = {
    val len = Bits(6 bits)

    when(frameControl.TYPE === B"01") {
        switch(frameControl.SUBTYPE) {
          is(B"0011") {len := 16} //TACK
          is(B"0100") {len := 16} //BFR
          is(B"0101") {len := 16} //VHT NDPA
          is(B"0110") {len := 16} //Ctrl EXT
          is(B"0111") {len := 16} //Ctrl Wrapper
          is(B"1000") {len := 16} //BAR
          is(B"1001") {len := 16} //BA
          is(B"1010") {len := 16} //PS-POLL
          is(B"1011") {len := 16} //RTS
          is(B"1100") {len := 10} //CTS
          is(B"1101") {len := 10} //ACK
          is(B"1110") {len := 16} //CF-END
          default {len := 16}
        }
    } .elsewhen(frameControl.TYPE === B"10") {
      val addrlen =  Bits(6 bits)
      when(frameControl.TODS.asBool && frameControl.FROMDS.asBool) {
        addrlen := 24
      } .otherwise {
        addrlen := 18
      }
      len :=  (6 + addrlen.asUInt + (frameControl.TYPE(3).asBits << 1).asUInt + (frameControl.HTC << 2).asUInt).asBits.resize(6)
    } .otherwise {
      len :=  (6 + 18 + (frameControl.HTC << 2).asUInt).asBits.resize(6)
    }
    len
  }

}

