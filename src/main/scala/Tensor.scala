package FPUv2

import chisel3._
import chisel3.util._
import FPUv2.utils._
import fudian._

abstract class TCPipelineModule(len: Int, ctrlGen: Data)
  extends FPUPipelineModule(len, ctrlGen)

class TCAddPipe(expWidth: Int, precision: Int, ctrlGen: Data = EmptyFPUCtrl())
  extends TCPipelineModule(expWidth + precision, ctrlGen){

  override def latency = 2
  val len = expWidth + precision
  val s1 = Module(new FCMA_ADD_s1(expWidth, precision, precision))
  val s2 = Module(new FCMA_ADD_s2(expWidth, precision))

  s1.io.a := S1Reg(io.in.bits.a)
  s1.io.b := S1Reg(io.in.bits.b)
  s1.io.rm := S1Reg(io.in.bits.rm)

  s1.io.b_inter_valid := false.B
  s1.io.b_inter_flags := 0.U.asTypeOf(s1.io.b_inter_flags)

  s2.io.in := S2Reg(s1.io.out)
  io.out.bits.result := s2.io.result
  io.out.bits.fflags := s2.io.fflags
  io.out.bits.ctrl.foreach( _ := S2Reg(S1Reg(io.in.bits.ctrl.get)))
}

class TCMulPipe(expWidth: Int, precision: Int, ctrlGen: Data = EmptyFPUCtrl())
  extends TCPipelineModule(expWidth + precision, ctrlGen){

  override def latency = 2
  val len = expWidth + precision

  //val multiplier = Module(new Multiplier(precision + 1, pipeAt = Seq(1)))
  val multiplier = Module(new NaiveMultiplier(precision + 1, pipeAt = Seq(1)))

  val s1 = Module(new FMUL_s1(expWidth, precision))
  val s2 = Module(new FMUL_s2(expWidth, precision))
  val s3 = Module(new FMUL_s3(expWidth, precision))

  s1.io.a := io.in.bits.a
  s1.io.b := io.in.bits.b
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

class TCCtrl(len: Int, depth_warp: Int) extends Bundle{
  val reg_idxw = UInt(5.W)
  val warpID = UInt(depth_warp.W)
}

class DotProdCtrl(len: Int, tcCtrl: Data = EmptyFPUCtrl()) extends Bundle{
  val rm = UInt(3.W)
  val c = UInt(len.W)
  val ctrl = FPUCtrlFac(tcCtrl)
}

class TCDotProductInput(DimN: Int, len: Int, tcCtrl: Data = EmptyFPUCtrl()) extends Bundle{
  val a = Vec(DimN, UInt(len.W))
  val b = Vec(DimN, UInt(len.W))
  //val ctrl = FPUCtrlFac(ctrlGen)
  val c = UInt(len.W)
  val rm = UInt(3.W)
  val ctrl = FPUCtrlFac(tcCtrl) // for TCCtrl
}

class TCDotProductOutput(len: Int, ctrlGen: Data) extends FPUOutput(len, ctrlGen)

class TCDotProduct(DimN: Int, expWidth: Int, precision: Int,
                   tcCtrl: Data = EmptyFPUCtrl()) extends Module{
  assert(isPow2(DimN) && DimN > 1)

  val len = expWidth + precision
  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(new TCDotProductInput(DimN, len, tcCtrl)))
    val out = DecoupledIO(new TCDotProductOutput(len, tcCtrl))
  })

  def addTree = {
    var vl = DimN
    var adds: Seq[Seq[TCAddPipe]] = Nil
    while (vl > 1) {
      vl = vl / 2
      adds = adds :+ Seq(Module(new TCAddPipe(expWidth, precision, new DotProdCtrl(len, tcCtrl)))) ++
        Seq.fill(vl - 1)(Module(new TCAddPipe(expWidth, precision)))
    }
    adds
  }

  val muls = Seq(Module(new TCMulPipe(expWidth, precision, new DotProdCtrl(len, tcCtrl)))) ++
    Seq.fill(DimN - 1)(Module(new TCMulPipe(expWidth, precision)))
  // connect IN and MULS
  val mctrl = Wire(new DotProdCtrl(len, tcCtrl))
  mctrl.rm := io.in.bits.rm
  mctrl.c := io.in.bits.c
  mctrl.ctrl.foreach( _ := io.in.bits.ctrl.get )
  (0 until DimN).foreach{ i =>
    muls(i).io.in.bits.a := io.in.bits.a(i)
    muls(i).io.in.bits.b := io.in.bits.b(i)
    muls(i).io.in.bits.c := DontCare
    muls(i).io.in.bits.op := DontCare
    muls(i).io.in.bits.rm := mctrl.rm
    muls(i).io.in.bits.ctrl.foreach{ _ := mctrl }
    muls(i).io.in.valid := io.in.valid
    io.in.ready := muls(i).io.in.ready
  }
  val adds = addTree
  val actrls = Seq.fill(log2Ceil(DimN))(Wire(new DotProdCtrl(len, tcCtrl)))
  // connect MULS and ADDS
  (0 until DimN / 2).foreach{ i =>
    adds(0)(i).io.in.bits.a := muls(i).io.out.bits.result
    adds(0)(i).io.in.bits.b := muls(i + DimN/2).io.out.bits.result
    adds(0)(i).io.in.bits.c := DontCare
    adds(0)(i).io.in.bits.op := DontCare
    adds(0)(i).io.in.bits.rm := actrls(0).rm
    adds(0)(i).io.in.bits.ctrl.foreach( _ := muls(i).io.out.bits.ctrl.get )
    adds(0)(i).io.in.valid := muls(i).io.out.valid
    muls(i).io.out.ready := adds(0)(i).io.in.ready
    muls(i + DimN/2).io.out.ready := adds(0)(i).io.in.ready
  }
  private var vl = DimN; private var i = 0;
  while(vl > 1) {
    vl = vl / 2
    actrls(i) := adds(i)(0).io.in.bits.ctrl.get
    if (i != 0) {
      // connect ADDS
      (0 until vl).foreach { j =>
        adds(i)(j).io.in.bits.a := adds(i - 1)(j).io.out.bits.result
        adds(i)(j).io.in.bits.b := adds(i - 1)(j + vl).io.out.bits.result
        adds(i)(j).io.in.bits.c := DontCare
        adds(i)(j).io.in.bits.op := DontCare
        adds(i)(j).io.in.bits.rm := actrls(i).rm
        adds(i)(j).io.in.bits.ctrl.foreach(_ := adds(i - 1)(j).io.out.bits.ctrl.get)

        adds(i)(j).io.in.valid := adds(i - 1)(j).io.out.valid
        adds(i - 1)(j).io.out.ready := adds(i)(j).io.in.ready
        adds(i - 1)(j + vl).io.out.ready := adds(i)(j).io.in.ready
      }
    }
    i = i + 1
  }

  val finalAdd = Module(new TCAddPipe(expWidth, precision, new DotProdCtrl(len, tcCtrl)))
  val outpack = Wire(new DotProdCtrl(len, tcCtrl))
  outpack := adds.last.head.io.out.bits.ctrl.get
  finalAdd.io.in.bits.a := adds.last.head.io.out.bits.result
  finalAdd.io.in.bits.b := outpack.c
  finalAdd.io.in.bits.c := DontCare
  finalAdd.io.in.bits.op := DontCare
  finalAdd.io.in.bits.rm := outpack.rm
  finalAdd.io.in.bits.ctrl.foreach( _ := outpack )
  finalAdd.io.in.valid := adds.last.head.io.out.valid
  adds.last.head.io.out.ready := finalAdd.io.in.ready

  val fifo = Module(new Queue(new TCDotProductOutput(len, tcCtrl), entries = 1, pipe = true))
  fifo.io.enq.bits.result := finalAdd.io.out.bits.result
  fifo.io.enq.bits.fflags := finalAdd.io.out.bits.fflags

  val outpack2 = Wire(new DotProdCtrl(len, tcCtrl))
  outpack2 := finalAdd.io.out.bits.ctrl.get
  fifo.io.enq.bits.ctrl.foreach( _ := outpack2.ctrl.get )
  fifo.io.enq.valid := finalAdd.io.out.valid
  finalAdd.io.out.ready := fifo.io.enq.ready
  io.out <> fifo.io.deq
}

class TensorCoreInput(vl: Int, tcCtrl: TCCtrl) extends Bundle{
  val data = Vec(vl, new FPUInput(32, EmptyFPUCtrl(), false))
  val ctrl = tcCtrl.cloneType
}
class TensorCoreOutput(vl:Int, tcCtrl: TCCtrl) extends Bundle{
  val data = Vec(vl, new FPUOutput(32, EmptyFPUCtrl()))
  val ctrl = tcCtrl.cloneType
}

class TensorCoreFP32(vl: Int, DimM: Int, DimN: Int, DimK:Int, tcCtrl: TCCtrl) extends Module {
  assert(DimM * DimN <= vl)
  assert(DimN * DimK <= vl)
  assert(DimM * DimK <= vl)
  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(new TensorCoreInput(vl, tcCtrl)))
    val out = DecoupledIO(new TensorCoreOutput(vl, tcCtrl))
  })
  val TCArray = Seq(Module(new TCDotProduct(DimN, 8, 24, tcCtrl))) ++
    Seq.fill(DimM*DimK-1)(Module(new TCDotProduct(DimN, 8, 24)))

  for (i <- 0 until vl) {
    io.out.bits.data(i).result := 0.U
    io.out.bits.data(i).fflags := 0.U
  }
  io.in.ready := TCArray.head.io.in.ready
  io.out.valid := TCArray.head.io.out.valid

  for(m <- 0 until DimM){
    for(k <- 0 until DimK){
      for(n <- 0 until DimN){
        TCArray(m * DimK + k).io.in.bits.a(n) := io.in.bits.data(m * DimN + n).a
        TCArray(m * DimK + k).io.in.bits.b(n) := io.in.bits.data(k * DimN + n).b
        TCArray(m * DimK + k).io.in.bits.c := io.in.bits.data(m * DimK + k).c
      }
      TCArray(m * DimK + k).io.in.bits.rm := io.in.bits.data(0).rm
      TCArray(m * DimK + k).io.in.bits.ctrl.foreach(_ := io.in.bits.ctrl)
      TCArray(m * DimK + k).io.in.valid := io.in.valid
      TCArray(m * DimK + k).io.out.ready := io.out.ready

      io.out.bits.data(m * DimK + k).result := TCArray(m * DimK + k).io.out.bits.result
      io.out.bits.data(m * DimK + k).fflags := TCArray(m * DimK + k).io.out.bits.fflags
    }
  }
  io.out.bits.ctrl := TCArray.head.io.out.bits.ctrl.get
}
