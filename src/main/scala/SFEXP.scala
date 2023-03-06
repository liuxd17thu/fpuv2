package FPUv2

import chisel3._
import chisel3.util._
import fudian.{FCMA_ADD_s1, FCMA_ADD_s2, FMULToFADD, FMUL_s1, FMUL_s2, FMUL_s3, RawFloat}
import FPUv2.utils._
import FPUv2.utils.FPUOps._

class SimpMulPipeFP32(ctrlGen: Data = EmptyFPUCtrl())
  extends FPUPipelineModule(32, ctrlGen){
  override def latency: Int = 2
  val expWidth: Int = 8; val precision: Int = 24

  //val multiplier = Module(new Multiplier(precision + 1, pipeAt = Seq(1)))
  val multiplier = Module(new NaiveMultiplier(precision + 1, pipeAt = Seq(1)))
  val s1 = Module(new FMUL_s1(expWidth, precision))
  val s2 = Module(new FMUL_s2(expWidth, precision))
  val s3 = Module(new FMUL_s3(expWidth, precision))

  val invProd = withInvProd(io.in.bits.op)

  s1.io.a := io.in.bits.a
  s1.io.b := Mux(invProd, invertSign(io.in.bits.b), io.in.bits.b)
  s1.io.rm := io.in.bits.rm

  s2.io.in := S1Reg(s1.io.out)
  s2.io.prod := multiplier.io.result
  s3.io.in := S2Reg(s2.io.out)

  val raw_a = RawFloat.fromUInt(s1.io.a, s1.expWidth, s1.precision)
  val raw_b = RawFloat.fromUInt(s1.io.b, s1.expWidth, s1.precision)

  multiplier.io.a := raw_a.sig
  multiplier.io.b := raw_b.sig
  multiplier.io.regEnables(0) := regEnable(1)

  io.out.bits.result := s3.io.result
  io.out.bits.fflags := s3.io.fflags
  io.out.bits.ctrl.foreach( _ := S2Reg(S1Reg(io.in.bits.ctrl.get)) )
}

class SimpAddPipeFP32(ctrlGen: Data = EmptyFPUCtrl())
  extends FPUPipelineModule(32, ctrlGen){
  override def latency: Int = 2
  val expWidth = 8; val precision = 24

  val s1 = Module(new FCMA_ADD_s1(expWidth, 2 * precision, precision))
  val s2 = Module(new FCMA_ADD_s2(expWidth, precision))

  val invAdd = withSUB(io.in.bits.op)

  val add1 = Cat(io.in.bits.a(31, 0), 0.U(precision.W))
  val add2 = Cat(
    Mux(invAdd, invertSign(io.in.bits.b), io.in.bits.b),
    0.U(precision.W)
  )
  s1.io.a := add1
  s1.io.b := add2
  s1.io.b_inter_valid := false.B
  s1.io.b_inter_flags := 0.U.asTypeOf(s1.io.b_inter_flags)
  s1.io.rm := io.in.bits.rm
  s2.io.in := S1Reg(s1.io.out)

  io.out.bits.result := s2.io.result
  io.out.bits.fflags := s2.io.fflags
  io.out.bits.ctrl.foreach( _ := S1Reg(io.in.bits.ctrl.get) )
}

class SFExpFP32(ctrlGen: Data = EmptyFPUCtrl()) extends Module{
  val io = IO(new Bundle{
    val in = new FPUInput(32, ctrlGen)
    val out = new FPUOutput(32, ctrlGen)
  })

}
