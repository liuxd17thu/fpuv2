package FPUv2

import chisel3._
import chisel3.util._
import fudian._
import FPUv2.utils._
import FPUv2.utils.FPUOps._

class IntToFP(ctrlGen: Data = EmptyFPUCtrl(), isFP64: Boolean = false)
  extends FPUPipelineModule(64, ctrlGen) {
  override def latency = 2

  val I2FCore = ( // support: I / IU / L / LU -> (FP32 | FP64)
    Module(new IntToFP_prenorm),
    if(isFP64) Module(new IntToFP_postnorm(8, 24)) else Module(new IntToFP_postnorm(11, 53))
  )

  I2FCore._1.io.in.int := io.in.bits.a
  I2FCore._1.io.in.sign := signedConvert(io.in.bits.op)
  I2FCore._1.io.in.long := longConvert(io.in.bits.op)

  I2FCore._2.io.in := S1Reg(I2FCore._1.io.out)
  I2FCore._2.io.rm := S1Reg(io.in.bits.rm)

  io.out.bits.result := S2Reg(I2FCore._2.io.result)
  io.out.bits.fflags := S2Reg(I2FCore._2.io.fflags)
  io.out.bits.ctrl.foreach( _ := S2Reg(S1Reg(io.in.bits.ctrl.get)) )
}
