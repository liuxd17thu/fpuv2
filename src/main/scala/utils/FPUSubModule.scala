package FPUv2.utils

import chisel3._
import chisel3.util._

// import parameters._

// comment these when using in GPGPU
object parameters {
  val depth_warp = 4
  val softThread = 12
  val hardThread = 4
}

import parameters._
// end

trait HasUIntToSIntHelper {
  implicit class UIntToSIntHelper(x: UInt) {
    def toSInt: SInt = Cat(0.U(1.W), x).asSInt
  }
}

class FPUCtrl(valid: Boolean = true) extends Bundle {
  val regIndex = UInt(5.W)
  val warpID = UInt(depth_warp.W)
  val vecMask = UInt(softThread.W)
  val wvd = Bool()
  val wxd = Bool()
}

class FPUInput(len: Int, hasCtrl: Boolean = false, topInput: Boolean = false) extends Bundle {
  val op = if(topInput) UInt(6.W) else UInt(3.W)
  val a, b, c = UInt(len.W)
  val rm = UInt(3.W)
  //val ctrl = if (hasCtrl) new FPUCtrl else new FPUCtrl(false)
  val ctrl = if(hasCtrl) Some(new FPUCtrl) else None
}

class vecFPUInput(softThread: Int, len: Int) extends Bundle {
  val data = Vec(softThread, new FPUInput(len, false, true))
  val ctrl = new FPUCtrl
}

class FPUOutput(len: Int, hasCtrl: Boolean = false) extends Bundle {
  val result = UInt(len.W)
  val fflags = UInt(5.W)
  //val ctrl = if (hasCtrl) new FPUCtrl else new FPUCtrl(false)
  val ctrl = if(hasCtrl) Some(new FPUCtrl) else None
}

abstract class FPUSubModule(len: Int, hasCtrl: Boolean = false) extends Module
  with HasUIntToSIntHelper {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Input(new FPUInput(len, hasCtrl))))
    val out = DecoupledIO(Output(new FPUOutput(len, hasCtrl)))
  })

  def invertSign(x: UInt): UInt = {
    Cat(~(x.head(1)), x.tail(1))
  }
}

abstract class FPUPipelineModule(len: Int, hasCtrl: Boolean = false)
  extends FPUSubModule(len, hasCtrl)
    with HasPipelineReg
