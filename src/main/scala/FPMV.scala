package FPUv2

import chisel3._
import chisel3.util._
import fudian._
import FPUv2.utils._
import FPUv2.utils.FPUOps._

class FPMV(expWidth: Int, precision: Int, ctrlGen: Data = EmptyFPUCtrl())
  extends FPUPipelineModule(expWidth + precision, ctrlGen) {
  override def latency = 2

  val op = io.in.bits.op
  val resSign = MuxLookup(op, io.in.bits.a.head(1), Seq(
    FN_FSGNJ(2, 0) -> io.in.bits.b.head(1),
    FN_FSGNJN(2, 0) -> !io.in.bits.b.head(1),
    FN_FSGNJX(2, 0) -> (io.in.bits.a.head(1) ^ io.in.bits.b.head(1))
  ))
  val a = S1Reg(Mux(op === FN_FCLASS(2, 0),
    io.in.bits.a,
    Cat(resSign, io.in.bits.a.tail(1))
  ))

  def classify(x: UInt, expWidth: Int, precision: Int): UInt = {
    val float = fudian.FloatPoint.fromUInt(x, expWidth, precision)
    val decode = float.decode
    val isNormal = !decode.expIsOnes && !decode.expIsZero
    Cat(
      decode.isQNaN,
      decode.isSNaN,
      decode.isInf && !float.sign,
      isNormal && !float.sign,
      decode.isSubnormal && !float.sign,
      decode.isZero && !float.sign,
      decode.isZero && float.sign,
      decode.isSubnormal && float.sign,
      isNormal && float.sign,
      decode.isInf && float.sign
    )
  }

  val s1_op = S1Reg(op)
  val classifyOut = classify(a, expWidth, precision)
  io.out.bits.result := S2Reg(Mux(s1_op === FN_FCLASS(2, 0), classifyOut, a))
  io.out.bits.fflags := 0.U(5.W)
  io.out.bits.ctrl.foreach( _ := S2Reg(S1Reg(io.in.bits.ctrl.get)))
}
