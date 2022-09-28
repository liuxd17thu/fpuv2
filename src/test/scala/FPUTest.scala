package FPUv2

import FPUv2.utils.{FPUCtrl, FPUInput, vecFPUInput}
import FPUv2.utils.FPUOps._
import FPUv2.utils.RoundingModes._
import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FPUTest extends AnyFlatSpec with ChiselScalatestTester {
  import TestArgs._
  object FPUModuleInput { d: FPUInput =>
    var count = 0
    def reset = { count = 0 }
    def apply(a: AnyVal, b: AnyVal, c: AnyVal, op: UInt, rm: UInt = RTZ) = {
      count = (count + 1) % 32
      (new FPUInput(32, true, true)).Lit(
        _.a -> toUInt(a).U,
        _.b -> toUInt(b).U,
        _.c -> toUInt(c).U,
        _.op -> op,
        _.rm -> rm,
        _.ctrl -> (new FPUCtrl).Lit(
          _.regIndex -> count.U,
          _.vecMask -> 0.U,
          _.warpID -> 0.U,
          _.wfd -> false.B,
          _.wxd -> false.B
        )
      )
    }
  }
  behavior of "FPU"
  it should "FPU Operations" in {
    test(new ScalarFPU(expWidth, precision, true)).withAnnotations(Seq(WriteVcdAnnotation)) { d =>
      FPUModuleInput.reset
      d.io.in.initSource()
      d.io.in.setSourceClock(d.clock)
      d.io.out.initSink()
      d.io.out.setSinkClock(d.clock)
      d.clock.setTimeout(20)
      d.io.out.ready.poke(true.B)
      fork{
        d.io.in.enqueueSeq(Seq(
          FPUModuleInput(1.0f, 10.0f, 1.0f, FN_FMADD),
          FPUModuleInput(20, 0, 0, FN_I2F),
          FPUModuleInput(3.0f, 37.0f, 0, FN_FADD),
          FPUModuleInput(4.0f, 10.0f, 0, FN_FLT),
          FPUModuleInput(5.0f, 0.0f, 0, FN_F2I),
          FPUModuleInput(6.0f, 10.0f, 0, FN_FCLASS)//,
          //subModuleInput(77.0f, 0, 0, FN_F2I),
          //subModuleInput(88, 0, 0, FN_I2F)
        ))
      }.fork {
        d.io.out.ready.poke(true.B)
      }.join()
      d.clock.step(10)
    }
  }

  behavior of "VectorFPU"
  it should "Split and Gather" in {
    test(new VectorFPU(expWidth, precision, 9, 3)).withAnnotations(Seq(WriteVcdAnnotation)){ d =>
      var count = 0
      def vecInput(a: AnyVal, b: AnyVal, c: AnyVal, op: UInt, mask: UInt){
        new vecFPUInput(9, 32).Lit(

        )
      }
    }
  }
}
