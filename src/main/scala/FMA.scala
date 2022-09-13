package FPUv2

import chisel3._
import chisel3.util._
import fudian.utils._
import fudian.{FCMA_ADD_s1, FCMA_ADD_s2, FMULToFADD, FMUL_s1, FMUL_s2, FMUL_s3, RawFloat}
import FPUv2.utils._
import FPUv2.utils.FPUOps._

class MulToAddIO(expWidth: Int, precision: Int, hasCtrl: Boolean = false) extends Bundle {
  val mulOutput = new FMULToFADD(expWidth, precision)
  val addAnother = UInt((expWidth + precision).W)
  val op = UInt(3.W)
  val ctrl = if (hasCtrl) Some(new FPUCtrl) else None
}

class FMULPipe(expWidth: Int, precision: Int, hasCtrl: Boolean = false)
  extends FPUPipelineModule(expWidth, precision, hasCtrl) {
  override def latency: Int = 3

  val toAdd = new MulToAddIO(expWidth, precision)

  val multiplier = Module(new Multiplier(precision + 1, pipeAt = Seq(1)))
  val s1 = Module(new FMUL_s1(expWidth, precision))
  val s2 = Module(new FMUL_s2(expWidth, precision))
  val s3 = Module(new FMUL_s3(expWidth, precision))

  val invProd = io.in.bits.op === FN_FNMADD || io.in.bits.op === FN_FNMSUB

  s1.io.a := io.in.bits.a
  s1.io.b := Mux(invProd, io.in.bits.b, invertSign(io.in.bits.b))
  s1.io.rm := io.in.bits.rm

  s2.io.in := S1Reg(s1.io.out)
  s2.io.prod := multiplier.io.result
  s3.io.in := S2Reg(s2.io.out)

  val raw_a = RawFloat.fromUInt(s1.io.a, s1.expWidth, s1.precision)
  val raw_b = RawFloat.fromUInt(s1.io.b, s1.expWidth, s1.precision)

  multiplier.io.a := raw_a
  multiplier.io.b := raw_b
  multiplier.io.regEnables(0) := regEnable(1)

  toAdd.addAnother := S2Reg(S1Reg(io.in.bits.c))
  /////////////////
  toAdd.ctrl.foreach(
    _ := S2Reg(S1Reg(
      io.in.bits.ctrl.getOrElse(0.U.asTypeOf(new FPUCtrl))
    ))
  )
  /////////////////
  toAdd.mulOutput := s3.io.to_fadd
  toAdd.op := S2Reg(S1Reg(io.in.bits.op))
  io.out.bits.result := s3.io.result
  io.out.bits.fflags := s3.io.fflags
  io.out.bits.ctrl.foreach(_ := toAdd.ctrl.getOrElse(0.U.asTypeOf(new FPUCtrl)))
  //  io.out.bits.ctrl.foreach(
  //    _ := S2Reg(S1Reg(
  //      io.in.bits.ctrl.getOrElse(0.U.asTypeOf(new FPUCtrl))
  //    ))
  //  )
}

class FADDPipe(expWidth: Int, precision: Int) extends FPUPipelineModule(expWidth, precision) {
  override def latency: Int = 2

  val len = expWidth + precision

  val fromMul = IO(Input(new MulToAddIO(expWidth, precision)))

  val s1 = Module(new FCMA_ADD_s1(expWidth, 2 * precision, precision))
  val s2 = Module(new FCMA_ADD_s2(expWidth, precision, precision))

  val isFMA = io.in.bits.op(2) === 1.U
  val s1_isFMA = S1Reg(isFMA)

  val s1_mulProd = S1Reg(fromMul.mulOutput)
  val srcA = S1Reg(io.in.bits.a)
  val srcB = S1Reg(Mux(isFMA, fromMul.addAnother, io.in.bits.b))

  val invAdd = io.in.bits.op(0) === 1.U

  val add1 = Mux(s1_isFMA,
    s1_mulProd.fp_prod.asUInt,
    Cat(srcA(len - 1, 0), 0.U(precision.W))
  )
  val add2 = Cat(
    Mux(invAdd, invertSign(srcB), srcB),
    0.U(precision.W)
  )
  s1.io.a := add1
  s1.io.b := add2
  s1.io.b_inter_valid := s1_isFMA
  s1.io.b_inter_flags := Mux(s1_isFMA,
    s1_mulProd.inter_flags,
    0.U.asTypeOf(s1.io.b_inter_flags)
  )
  s1.io.rm := S1Reg(io.in.bits.rm)
  s2.io.in := S2Reg(s1.io.out)
  ////////////////////////
  io.out.bits.ctrl.foreach(
    _ := S2Reg(S1Reg(
      io.in.bits.ctrl.getOrElse(0.U.asTypeOf(new FPUCtrl))
    ))
  )
  ////////////////////////
  io.out.bits.result := s2.io.result
  io.out.bits.fflags := s2.io.fflags
}

class FMA(expWidth: Int, precision: Int, hasCtrl: Boolean = false)
  extends FPUSubModule(expWidth, precision) {

  val mulPipe = Module(new FMULPipe(expWidth, precision))
  val addPipe = Module(new FADDPipe(expWidth, precision))

  // 加法器从FMA输入端和乘法器输出端接收数据
  // 乘加和加法同时抵达时，乘加优先级更高: 0->输入来自乘法器输出, 1->输入来自外层输入
  val toAddArbiter = Module(new Arbiter(new Bundle {
    val op = UInt(3.W)
    val ctrl = if (hasCtrl) Some(new FPUCtrl) else None
  }, 2))
  toAddArbiter.io.in(1).bits.op := io.in.bits.op.tail(3)
  toAddArbiter.io.in(1).bits.ctrl.foreach(_ := io.in.bits.ctrl.getOrElse(0.U.asTypeOf(new FPUCtrl)))
  toAddArbiter.io.in(0).bits.op := mulPipe.toAdd.op
  toAddArbiter.io.in(0).bits.ctrl.foreach(_ := mulPipe.toAdd.ctrl.getOrElse(0.U.asTypeOf(new FPUCtrl)))
  toAddArbiter.io.in(1).valid := FPUOps.isADDSUB(io.in.bits.op) && io.in.valid
  toAddArbiter.io.in(0).valid := FPUOps.isFMA(mulPipe.toAdd.op) && mulPipe.io.out.valid

  addPipe.io.in.bits.op := toAddArbiter.io.out.bits.op
  addPipe.io.in.bits.ctrl.foreach(_ := toAddArbiter.io.out.bits.ctrl.getOrElse(0.U.asTypeOf(new FPUCtrl)))
  toAddArbiter.io.out.ready := addPipe.io.in.ready

  // 加法为乘加让行的同时也会阻塞FMA输入，确保自己之后能够进入流水线
  // 另一种阻塞FMA输入的情况是乘法器那边卡住了
  io.in.ready := mulPipe.io.in.ready && !(toAddArbiter.io.in(1).valid && !toAddArbiter.io.in(1).ready)
  val mulFIFO = Module(new Queue(new FPUOutput(expWidth, precision), entries = 1, pipe = true))
  val addFIFO = Module(new Queue(new FPUOutput(expWidth, precision), entries = 1, pipe = true))
  mulFIFO.io.enq.bits := mulPipe.io.out.bits
  mulFIFO.io.enq.valid := mulPipe.io.out.valid && FPUOps.isFMUL(mulPipe.toAdd.op)
  addFIFO.io.enq <> addPipe.io.out

  mulPipe.io.out.ready := (toAddArbiter.io.in(0).ready && FPUOps.isFMA(mulPipe.toAdd.op)) ||
    (mulFIFO.io.enq.ready && FPUOps.isFMUL(mulPipe.toAdd.op))

  // FMA输出端从乘法输出端和加法输出端接收数据，加法(乘加)优先级更高
  val toOutArbiter = Module(new Arbiter(new FPUOutput(expWidth, precision), 2))
  toOutArbiter.io.in(0) <> addFIFO.io.deq
  toOutArbiter.io.in(1) <> mulFIFO.io.deq
  io.out <> toOutArbiter.io.out
}
