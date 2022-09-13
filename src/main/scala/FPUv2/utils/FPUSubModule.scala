package FPUv2.utils

import chisel3._
import chisel3.util._

// import parameters._

// comment these when using in GPGPU
object parameters {
  val depth_warp = 4
  val num_thread = 8
}

import parameters._
// end

trait HasUIntToSIntHelper {
  implicit class UIntToSIntHelper(x: UInt) {
    def toSInt: SInt = Cat(0.U(1.W), x).asSInt
  }
}

class FPUCtrl extends Bundle {
  val regIndex = UInt(5.W)
  val warpID = UInt(depth_warp.W)
  val vecMask = UInt(num_thread.W)
  val wfd = Bool()
  val wxd = Bool()
}

class FPUOutput(expWidth: Int, precision: Int, hasCtrl: Boolean = false) extends Bundle {
  val result = Output(UInt((expWidth + precision).W))
  val fflags = Output(UInt(5.W))
  val ctrl = if(hasCtrl) Some(Output(new FPUCtrl)) else None
}

abstract class FPUSubModule(expWidth: Int, precision: Int, hasCtrl: Boolean = false) extends Module
  with HasUIntToSIntHelper {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new Bundle {
      val op = Input(UInt(3.W))
      val a, b, c = Input(UInt((expWidth + precision).W))
      val rm = Input(UInt(3.W))
      val ctrl = if(hasCtrl) Some(Input(new FPUCtrl)) else None
    }))
    val out = DecoupledIO(new FPUOutput(expWidth, precision, hasCtrl))
  })

  def invertSign(x: UInt): UInt = {
    Cat(~(x.head(1)), x.tail(1))
  }
}

abstract class FPUPipelineModule(expWidth: Int, precision: Int, hasCtrl: Boolean = false)
  extends FPUSubModule(expWidth, precision, hasCtrl)
    with HasPipelineReg
