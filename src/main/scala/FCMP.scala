package FPUv2

import chisel3._
import chisel3.util._
import fudian._
import FPUv2.utils._
import FPUv2.utils.FPUOps._

class FCMP(expWidth: Int, precision: Int, ctrlGen: Data = new EmptyFPUCtrl)
  extends FPUPipelineModule(expWidth + precision, ctrlGen) {
  override def latency = 2

  val len = expWidth + precision
  val FCMPCore = Module(new fudian.FCMP(expWidth, precision))

  FCMPCore.io.a := io.in.bits.a
  FCMPCore.io.b := io.in.bits.b
  // TODO: signal logic
  FCMPCore.io.signaling := false.B
  val eq = S1Reg(FCMPCore.io.eq)
  val le = S1Reg(FCMPCore.io.le)
  val lt = S1Reg(FCMPCore.io.lt)
  val fflags = S1Reg(FCMPCore.io.fflags)
  val op = S1Reg(io.in.bits.op)
  val (a, b) = (S1Reg(io.in.bits.a), S1Reg(io.in.bits.b))
  val max = Mux(lt, b, a)
  val min = Mux(lt, a, b)

  val result = Mux(fflags.head(1).asBool,
    0.U(32.W),
    MuxLookup(Cat(1.U(3.W), op), 0.U(32.W), Seq(
      FN_FEQ -> eq.asUInt,
      FN_FNE -> (!eq).asUInt,
      FN_FLE -> le.asUInt,
      FN_FLT -> lt.asUInt,
      FN_MAX -> max,
      FN_MIN -> min
    ))
  )
  io.out.bits.ctrl.foreach( _ := S2Reg(S1Reg(io.in.bits.ctrl.get)) )
  io.out.bits.result := S2Reg(result)
  io.out.bits.fflags := S2Reg(fflags)
}
