package wifimac


import spinal.core.{True, _}
import spinal.lib._

import scala.util.Random

//Hardware definition
case class NavConfig(support2Nav:Boolean,supportTxNav:Boolean) {

}

case class NavIf(isTxNav:Boolean) extends Bundle with IMasterSlave {
  val txopCutDown = Bool()

  val setNav = Bool()
  val setNavValue = UInt(15 bits)
  val setTxopHolder = Bits(48 bits)
  val setNavHolderAc = if(isTxNav) Bits(4 bits) else null

  val navOP = NavOP(isTxNav)

  override def asMaster()={
    if(isTxNav) out(txopCutDown,setNav,setNavValue,setNavHolderAc,setTxopHolder)
    else out(txopCutDown,setNav,setNavValue,setTxopHolder)
    master(navOP)
  }
}

case class RXInfo() extends Bundle with  IMasterSlave {
  val RAMatch = Bool()
  val RxRA = Bits(48 bits)
  val RxSolicitIMR = Bool()
  val RxContainTrigger = Bool()
  val RxContainPSPOLL = Bool()
  val RxDid = Bits(16 bits)
  val IntraPPDU = Bool()
  val RxTxopDurtion = Bits(7 bits)
  val RxUseTxopDuration  = Bool()

  override def asMaster()={
    out(RAMatch,RxRA,RxSolicitIMR,RxContainTrigger,RxContainPSPOLL,RxDid,IntraPPDU,RxTxopDurtion,RxUseTxopDuration)
  }

}

case class TXInfo() extends Bundle with  IMasterSlave{
  val TxDid = Bits(16 bits)
  val PrimaryAc = Bits(4 bits)
  override def asMaster()={
    out(TxDid,PrimaryAc)
  }
}



case class NavOP(isTxNav:Boolean) extends Bundle with IMasterSlave{
  val resetNav = Bool()
  val navActive = Bool()
  val curTxopHolder = Bits(48 bits)
  val curNavValue = UInt(15 bits)
  val curNavHolderAc =if(isTxNav) Bits(4 bits) else null

  override def asMaster()={
    out(resetNav)
    if(isTxNav) {
      in(navActive, curTxopHolder, curNavValue, curNavHolderAc)
    } else {
      in(navActive, curTxopHolder, curNavValue)
     }
  }
}



case class navCounter(isTxNav:Boolean)extends Component{
  val io = new Bundle {
    val tick_1us = in Bits(1 bits)
    val navCutDown = in Bool()
    val navIf = slave(NavIf(isTxNav))
  }

  val navCnt = Reg(UInt(15 bits)) init 0

  val navHolderAc = if (isTxNav) Reg(Bits(4 bits)) init 0 else null

  val navTxopHolder = if (isTxNav) B"48'x0" else Reg(Bits(48 bits)) init 0

  if (isTxNav) {
    io.navIf.navOP.curNavHolderAc := navHolderAc
  }
  io.navIf.navOP.curNavValue := navCnt
  io.navIf.navOP.curTxopHolder := navTxopHolder
  io.navIf.navOP.navActive := navCnt =/= 0

  when(io.navIf.navOP.resetNav) {
    navCnt := 0
  } .elsewhen(io.navIf.setNav) {
    navCnt := io.navIf.setNavValue
  } .elsewhen(io.navCutDown && io.tick_1us.asBool) {
    navCnt := navCnt - 1
  }

  when(io.navIf.setNav) {
    if (isTxNav) {
      navHolderAc := io.navIf.setNavHolderAc
    } else {
      navTxopHolder := io.navIf.setTxopHolder
    }
  }
}



case class navUpdate(config:NavConfig) extends Component {
  val io = new Bundle {
    val tick_1us = in Bits (1 bit)
    val rxinfo = slave(RXInfo())
    val RxPPDUEnd = in Bool()
    val txinfo = if (config.supportTxNav) slave(TXInfo()) else null
    val TxPPDUEnd = if (config.supportTxNav) in Bool() else null

    val basicNavOp = slave(NavOP(false))
    val intraNavOp = if(config.support2Nav) slave(NavOP(false)) else null
    val txNavOp = if(config.supportTxNav) slave(NavOP(true)) else null
  }

  val basicNav = navCounter(false)
  val intraNav = if(config.support2Nav)  navCounter(false)  else null
  val txNav = if(config.supportTxNav) navCounter(true) else null

  val rxDidvalue:UInt = {
    val result = UInt(15 bits)
    val tmpresult = UInt(15 bits)
    tmpresult := 0
    when(io.rxinfo.RxUseTxopDuration) {
      when(io.rxinfo.RxTxopDurtion === 127) {
        tmpresult := 0
      } .elsewhen(io.rxinfo.RxTxopDurtion(0).asBits === B"0") {
        tmpresult := (io.rxinfo.RxTxopDurtion(6 downto 1).asUInt * 8).resize(15)
      } .otherwise {
        tmpresult := (io.rxinfo.RxTxopDurtion(6 downto 1).asUInt * 128 + 512).resize(15)
      }
      when(tmpresult > 8448) {
        result := 8448
      } .otherwise {
        result := tmpresult
      }
      result
    } .elsewhen(io.rxinfo.RxDid(15).asBits.asBool) {
      result := io.rxinfo.RxDid(14 downto 0).asUInt
    } .elsewhen(io.rxinfo.RxContainPSPOLL) { //PS-POLL sifs + acktime
      result := 16 + 44
    } .otherwise {
      result := 0
    }
    result
  }

  val updateBasicNav:Bool = {
    val result = Bool()
    if(config.support2Nav) {
      result := io.RxPPDUEnd && !io.rxinfo.RAMatch && !io.rxinfo.IntraPPDU && (rxDidvalue > basicNav.io.navIf.navOP.curNavValue)
    } else {
      result := io.RxPPDUEnd && !io.rxinfo.RAMatch && (rxDidvalue > basicNav.io.navIf.navOP.curNavValue)
    }
    result
  }

  basicNav.io.navIf.setNav := updateBasicNav
  basicNav.io.navIf.setNavValue := rxDidvalue
  basicNav.io.navIf.setTxopHolder := Mux(io.rxinfo.RxUseTxopDuration, B"48'x0", io.rxinfo.RxRA)
  basicNav.io.tick_1us := io.tick_1us
  basicNav.io.navCutDown := True



  val rxTriggerAndNotHolder = (io.rxinfo.RxContainTrigger ) && (if(config.supportTxNav) !txNav.io.navIf.navOP.navActive else True)
  val rxNoIMRAndNotHolder = (!io.rxinfo.RxSolicitIMR) && (if(config.supportTxNav) !txNav.io.navIf.navOP.navActive else True)

  val updateIntraNav:Bool = {
    val result = Bool()
    if(config.support2Nav) {
      result := io.RxPPDUEnd && ((!io.rxinfo.RAMatch) || rxTriggerAndNotHolder || rxNoIMRAndNotHolder)  && io.rxinfo.IntraPPDU && (rxDidvalue > intraNav.io.navIf.navOP.curNavValue)
    } else {
      result := False
    }
    result
  }

  if(config.support2Nav) {
    intraNav.io.navIf.setNav := updateIntraNav
    intraNav.io.navIf.setNavValue := rxDidvalue
    intraNav.io.navIf.setTxopHolder := Mux(io.rxinfo.RxUseTxopDuration, B"48'x0", io.rxinfo.RxRA)
    intraNav.io.tick_1us := io.tick_1us
    intraNav.io.navCutDown := True
  }




  val updateTxNav:Bool = {
    val result = Bool()
    if(config.supportTxNav) {
      result := io.TxPPDUEnd && !txNav.io.navIf.navOP.navActive && !io.txinfo.TxDid(15).asBits.asBool
    } else {
      result := False
    }
    result
  }

  if(config.supportTxNav) {
    txNav.io.navIf.setNav := updateTxNav
    txNav.io.navIf.setNavValue := io.txinfo.TxDid(14 downto 0).asUInt
    txNav.io.navIf.setNavHolderAc := io.txinfo.PrimaryAc
    txNav.io.tick_1us := io.tick_1us
    txNav.io.navCutDown := True
  }


  io.basicNavOp <> basicNav.io.navIf.navOP

  if(config.support2Nav) {
    io.intraNavOp <> intraNav.io.navIf.navOP
  }

  if (config.supportTxNav) {
    io.txNavOp <> txNav.io.navIf.navOP
  }

}