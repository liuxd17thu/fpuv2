package FPUv2

import FPUv2.utils._
import chisel3.{VecInit, _}
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
  assert(softThread % hardThread == 0)
  assert(softThread>2 && hardThread>1)

  val len = expWidth + precision
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new vecFPUInput(hardThread, len)))
    val out = DecoupledIO(new Bundle {
      val data = Vec(hardThread, new FPUOutput(64, false))
      val ctrl = new FPUCtrl
    })
  })

  val FPUArray = Range(0, hardThread).map{ i =>
    Module(new ScalarFPU(expWidth, precision, i==0))
  }

  if(softThread == hardThread){
    io.in.ready := FPUArray(0).io.in.ready
    FPUArray.zipWithIndex.foreach{ case (x, i) =>
      x.io.in.valid := io.in.valid
      x.io.in.bits := io.in.bits.data(i)
      if(i == 0)
        x.io.in.bits.ctrl := io.in.bits.ctrl
//      else
//        x.io.in.bits.ctrl := DontCare
    }

    io.out.valid := FPUArray(0).io.out.valid
    io.out.bits.ctrl := FPUArray(0).io.out.bits.ctrl
    FPUArray.zipWithIndex.foreach{ case (fpu, i) =>
      fpu.io.out.ready := io.out.ready
      io.out.bits.data(i) := fpu.io.in.bits
    }
  }
  else {
    //================ Result Sending ========
    val maxIter = softThread / hardThread
    val maskSlice = (1<<hardThread-1).U(softThread.W)
    val inReg = RegInit(VecInit.fill(softThread)(new FPUInput(len, false, true)))
    val inCtrlReg = RegInit(new FPUCtrl)

    //val sendCS = RegInit(0.U(log2Ceil(maxIter+1).W))
    val sendNS = WireInit(0.U(log2Ceil(maxIter+1).W))

    // process #1
    val sendCS = RegNext(sendNS)
    // process #2
    switch(sendCS){
      is(0.U){
        when(io.in.fire){
          sendNS := sendCS +% 1.U
        }.otherwise{ sendNS := 0.U }
      }
      is((1 until maxIter).map{_.U}){
        when(FPUArray(0).io.in.fire){
          sendNS := sendCS +% 1.U
        }.otherwise{sendNS := sendCS}
      }
      is(maxIter.U){
        when(FPUArray(0).io.in.fire){
          when(io.in.fire){
            sendNS := 1.U
          }.otherwise{
            sendNS := 0.U
          }
        }.otherwise{sendNS := sendCS}
      }
    }
    // process #3A
    switch(sendNS){
      is(0.U){
        inReg.foreach{ _ := 0.U.asTypeOf(inReg(0))}
        inCtrlReg := 0.U.asTypeOf(new FPUCtrl)
      }
      is(1.U){
        when(io.in.fire){
          (inReg zip io.in.bits.data).map { x => x._1 := x._2 }
          inCtrlReg := io.in.bits.ctrl
        }.otherwise{}
      }
      is((2 until maxIter).map(_.U)){
        when(FPUArray(0).io.in.fire){
          (0 until maxIter).foreach{
            i => inReg(i) := {
              if (i + hardThread < maxIter)
                inReg(i + hardThread)
              else
                0.U.asTypeOf(inReg(i))
            }
          }
        }.otherwise{}
      }
    }
    // process #3B
    (0 until hardThread).foreach{ i =>
      FPUArray(i).io.in.bits := inReg(i)
      FPUArray(0).io.in.bits.ctrl := inCtrlReg
      FPUArray(i).io.in.valid := sendCS =/= 0.U
    }

    //=============== Result Collecting ===================
    //val recvCS = RegInit(0.U(log2Ceil(maxIter+1).W))
    val recvNS = WireInit(0.U(log2Ceil(maxIter+1).W))
    val recvCS = RegNext(recvNS)
    val outReg = RegInit(VecInit.fill(softThread)(new FPUOutput(len, false)))
    val outCtrlReg = RegInit(new FPUCtrl)

    switch(recvCS){
      is(maxIter.U){
        when(io.out.fire){
          when(FPUArray(0).io.out.fire){
            recvNS := 1.U
          }.otherwise{
            recvNS := 0.U
          }
        }.otherwise{ recvNS := recvCS }
      }
      is((0 until maxIter).map(_.U)){
        when(FPUArray(0).io.out.fire){
          recvNS := recvCS +% 1.U
        }.otherwise{ recvNS := recvCS }
      }
    }

    switch(recvNS){
      is(0.U){
        outReg.foreach{ _ := 0.U.asTypeOf(outReg(0)) }
        outCtrlReg := 0.U.asTypeOf(outCtrlReg)
      }
      is((1 to maxIter).map(_.U)){
        when(FPUArray(0).io.out.fire){
          (0 until maxIter).foreach{ i =>
            outReg(i) := {
              if(i+hardThread<maxIter)
                outReg(i + maxIter)
              else
                FPUArray(i+hardThread-maxIter).io.out.bits
            }
          }
          outCtrlReg := Mux(recvNS===1.U, FPUArray(0).io.out.bits.ctrl, outCtrlReg)
        }.otherwise{}
      }
    }
    io.out.valid := recvCS === maxIter.U
    io.out.bits.ctrl := FPUArray(0).io.out.bits.ctrl
    (io.out.bits.data zip FPUArray).foreach{ case (data, fpu) =>
      data := fpu.io.out.bits
      fpu.io.out.ready := io.out.ready
    }
  }
}