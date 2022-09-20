package FPUv2

import FPUv2.utils.FPUSubModule
import chisel3._
import chisel3.util._

class ScalarFPU(expWidth: Int, precision: Int, hasCtrl: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new Bundle {
      val fpuop = Input(UInt(6.W))
      val a, b, c = Input(UInt(64.W))
      val rm = Input(UInt(3.W))
    }))
    val out = DecoupledIO(new Bundle {
      val result = Output(UInt(64.W))
      val flags = Output(UInt(5.W))
    })
  })
  val subModules = Array[FPUSubModule](
    Module(new FMA(expWidth, precision, hasCtrl)),
    Module(new FCMP(expWidth, precision, hasCtrl)),
    Module(new FPMV(expWidth, precision, hasCtrl)),
    Module(new FPToInt(hasCtrl)),
    Module(new IntToFP(hasCtrl))
  )
}
