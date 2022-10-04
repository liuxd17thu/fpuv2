package FPUv2

import chisel3._
import chisel3.util._
import fudian._
import FPUv2.utils._
import FPUv2.utils.FPUOps._

class IntToFP(ctrlGen: Data = EmptyFPUCtrl())
  extends FPUPipelineModule(64, ctrlGen) {
  override def latency = 2

  val isSingle = !doubleConvert(io.in.bits.op)
  val s2_isSingle = S2Reg(S1Reg(isSingle))

  val I2FCore = (
    Module(new IntToFP_prenorm),
    Module(new IntToFP_postnorm(8, 24))
  )

  //  val I2DCore = (
  //
  //  )

  I2FCore._1.io.in.int := Mux(isSingle, io.in.bits.a, 0.U(64.W))
  I2FCore._1.io.in.sign := signedConvert(io.in.bits.op) && isSingle
  I2FCore._1.io.in.long := longConvert(io.in.bits.op) && isSingle

  I2FCore._2.io.in := S1Reg(I2FCore._1.io.out)
  I2FCore._2.io.rm := S1Reg(io.in.bits.rm)

  io.out.bits.result := Mux(s2_isSingle, S2Reg(I2FCore._2.io.result), 0.U(64.W))
  io.out.bits.fflags := Mux(s2_isSingle, S2Reg(I2FCore._2.io.fflags), 0.U(5.W))
  io.out.bits.ctrl.foreach( _ := S2Reg(S1Reg(io.in.bits.ctrl.get)) )
}
