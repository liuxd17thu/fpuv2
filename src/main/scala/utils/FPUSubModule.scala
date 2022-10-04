package FPUv2.utils

import chisel3._
import chisel3.util._

// import parameters._

// comment these when using in GPGPU
//object parameters {
//  val depth_warp = 4
//  val softThread = 12
//  val hardThread = 4
//}

// end

trait HasUIntToSIntHelper {
  implicit class UIntToSIntHelper(x: UInt) {
    def toSInt: SInt = Cat(0.U(1.W), x).asSInt
  }
}

class EmptyFPUCtrl extends Bundle{
  val empty = UInt(0.W)
}
object EmptyFPUCtrl {
  def apply() = new EmptyFPUCtrl
}

class TestFPUCtrl(depth_warp: Int, softThread: Int) extends Bundle {
  val regIndex = UInt(5.W)
  val warpID = UInt(depth_warp.W)
  val vecMask = UInt(softThread.W)
  val wvd = Bool()
  val wxd = Bool()
}

object FPUCtrlFac{
  def apply[T<:Data](gen: T): Option[T] = {
    gen match {
      case _:EmptyFPUCtrl => None
      case _ => Some(gen.cloneType)
    }
  }
}


class FPUInput(len: Int, ctrlGen: Data = EmptyFPUCtrl(), topInput: Boolean = false) extends Bundle {
  val op = if(topInput) UInt(6.W) else UInt(3.W)
  val a, b, c = UInt(len.W)
  val rm = UInt(3.W)
  //val ctrl = if (hasCtrl) new FPUCtrl else new FPUCtrl(false)
  val ctrl = FPUCtrlFac(ctrlGen)
}

class vecFPUInput[T<: TestFPUCtrl](softThread: Int, len: Int, ctrlGen: T) extends Bundle {
  val data = Vec(softThread, new FPUInput(len, EmptyFPUCtrl(), true))
  val ctrl = ctrlGen.cloneType
}

class FPUOutput(len: Int, ctrlGen: Data = EmptyFPUCtrl()) extends Bundle {
  val result = UInt(len.W)
  val fflags = UInt(5.W)
  //val ctrl = if (hasCtrl) new FPUCtrl else new FPUCtrl(false)
  val ctrl = FPUCtrlFac(ctrlGen)
}

abstract class FPUSubModule(len: Int, ctrlGen: Data = EmptyFPUCtrl()) extends Module
  with HasUIntToSIntHelper {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Input(new FPUInput(len, ctrlGen))))
    val out = DecoupledIO(Output(new FPUOutput(len, ctrlGen)))
  })

  def invertSign(x: UInt): UInt = {
    Cat(~(x.head(1)), x.tail(1))
  }
}

abstract class FPUPipelineModule(len: Int, ctrlGen: Data = EmptyFPUCtrl())
  extends FPUSubModule(len, ctrlGen)
    with HasPipelineReg
