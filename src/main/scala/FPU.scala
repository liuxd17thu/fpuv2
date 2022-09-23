package FPUv2

import FPUv2.utils._
import chisel3._
import chisel3.util._

class ScalarFPU(expWidth: Int, precision: Int, hasCtrl: Boolean = false) extends Module {
  val len = expWidth + precision
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new FPUInput(len, hasCtrl, true)))
    val out = DecoupledIO(new FPUOutput(64, hasCtrl))
    val select = Output(UInt(3.W))
  })
  val subModules = Array[FPUSubModule](
    Module(new FMA(expWidth, precision, hasCtrl)),
    Module(new FCMP(expWidth, precision, hasCtrl)),
    Module(new FPMV(expWidth, precision, hasCtrl)),
    Module(new FPToInt(hasCtrl)),
    Module(new IntToFP(hasCtrl))
  )

  val fu = io.in.bits.op.head(3)
  for((module, idx) <- subModules.zipWithIndex){
    module.io.in.bits.op := Mux(idx.U===fu, io.in.bits.op(2, 0), 0.U(3.W))
    module.io.in.bits.rm := Mux(idx.U===fu, io.in.bits.rm, 0.U(3.W))
    module.io.in.bits.a := Mux(idx.U===fu, io.in.bits.a, 0.U(len.W))
    module.io.in.bits.b := Mux(idx.U===fu, io.in.bits.b, 0.U(len.W))
    module.io.in.bits.c := Mux(idx.U===fu, io.in.bits.c, 0.U(len.W))
    module.io.in.bits.ctrl := Mux(idx.U===fu, io.in.bits.ctrl, 0.U.asTypeOf(new FPUCtrl))
    module.io.in.valid := idx.U===fu
  }
  io.in.ready := MuxLookup(fu, false.B,
    subModules.zipWithIndex.map{ case (module, idx) =>
      idx.U -> module.io.in.ready
    }
  )

  val outArbiter = Module(new Arbiter(new FPUOutput(64, hasCtrl), 5))
  subModules.zipWithIndex.foreach{ case (module, idx) =>
    outArbiter.io.in(idx) <> module.io.out
  }
  io.out <> outArbiter.io.out
  io.select := outArbiter.io.chosen
}

class VectorFPU(expWidth: Int, precision: Int, softThread: Int = 32, hardThread: Int = 32) extends Module {
  val len = expWidth + precision
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new Bundle {

    }))
    val out = DecoupledIO(new Bundle {

    })
  })
}