package FPUv2

import chisel3._
import chisel3.util._
import FPUv2.utils._
import FPUv2.utils.FPUOps._

/* TODO:
*/
class FPToInt(ctrlGen: Data = EmptyFPUCtrl(), isFP64: Boolean = false)
  extends FPUPipelineModule(64, ctrlGen) {
  override def latency = 2

  val F2ICore = if(isFP64){
      Module(new fudian.FPToInt(11, 53)) // support: FP64 -> I / IU / L / LU
  } else{
      Module(new fudian.FPToInt(8, 24)) // support: FP32 -> I / IU / L / LU
  }
  //val isSingle = S1Reg(!doubleConvert(io.in.bits.op))
  val coreOp = S1Reg(io.in.bits.op(1, 0))
  val src = S1Reg(io.in.bits.a)
  val rm = S1Reg(io.in.bits.rm)

  F2ICore.io.op := coreOp
  F2ICore.io.a := { if(isFP64) src else src(31, 0) }
  F2ICore.io.rm := rm

  io.out.bits.fflags := S2Reg(F2ICore.io.fflags)
  io.out.bits.result := S2Reg(F2ICore.io.result)
  io.out.bits.ctrl.foreach(_ := S2Reg(S1Reg(io.in.bits.ctrl.get)))
}
