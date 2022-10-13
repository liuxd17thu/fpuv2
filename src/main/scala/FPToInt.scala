package FPUv2

import chisel3._
import chisel3.util._
import FPUv2.utils._
import FPUv2.utils.FPUOps._

/* TODO:
*/
class FPToInt(ctrlGen: Data = EmptyFPUCtrl())
  extends FPUPipelineModule(64, ctrlGen) {
  override def latency = 2

  val F2ICore = Module(new fudian.FPToInt(8, 24))
  //val D2ICore = Module(None)
  val isSingle = S1Reg(!doubleConvert(io.in.bits.op))
  val coreOp = S1Reg(io.in.bits.op(1, 0))
  val src = S1Reg(io.in.bits.a)
  val rm = S1Reg(io.in.bits.rm)

  F2ICore.io.op := coreOp
  F2ICore.io.a := Mux(isSingle, src(31, 0), 0.U(32.W))
  F2ICore.io.rm := rm

  io.out.bits.fflags := Mux(S2Reg(isSingle), S2Reg(F2ICore.io.fflags), 0.U(5.W))
  io.out.bits.result := Mux(S2Reg(isSingle), S2Reg(F2ICore.io.result), 0.U(64.W))
  io.out.bits.ctrl.foreach( _ := S2Reg(S1Reg(io.in.bits.ctrl.get)))
}
