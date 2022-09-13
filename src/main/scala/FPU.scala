package FPUv2

import chisel3._
import chisel3.util._

class ScalarFPU(expWidth: Int, precision: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new Bundle {
      val fpuop = Input(UInt(6.W))
      val a, b, c = Input(UInt((expWidth + precision).W))
      val rm = Input(UInt(3.W))
    }))
    val out = DecoupledIO(new Bundle {
      val result = Output(UInt((expWidth + precision).W))
      val flags = Output(UInt(5.W))
    })
    val select = Output(UInt(3.W))
  })
}
