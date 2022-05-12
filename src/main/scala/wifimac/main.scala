package wifimac

import spinal.core.SpinalConfig

object main {
  def main(args: Array[String]) {
    val config = NavConfig(support2Nav = true, supportTxNav = true)
    SpinalConfig(targetDirectory = "rtl").generateVerilog(navUpdate(config))
  }
}
