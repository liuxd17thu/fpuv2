package FPUv2

import FPUv2.utils.{EmptyFPUCtrl, FPUInput, TestFPUCtrl, vecFPUInput}
import FPUv2.utils.FPUOps._
import FPUv2.utils.RoundingModes._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals.AddVecLiteralConstructor
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import FPUv2.utils.TestFPUCtrl

object Parameters{
  val depthWarp: Int = 4
  val hardThread: Int = 4
  val softThread: Int = 12
}
import FPUv2.Parameters._
class FPUTest extends AnyFlatSpec with ChiselScalatestTester {
  import TestArgs._

  object FPUModuleInput { d: FPUInput =>
    var count = 0
    def reset = { count = 0 }
    def apply(a: AnyVal, b: AnyVal, c: AnyVal, op: UInt, rm: UInt = RTZ): FPUInput = {
      count = (count + 1) % 32
      (new FPUInput(32, new TestFPUCtrl(depthWarp, softThread), true)).Lit(
        _.a -> toUInt(a).U,
        _.b -> toUInt(b).U,
        _.c -> toUInt(c).U,
        _.op -> op,
        _.rm -> rm,
        _.ctrl.get -> (new TestFPUCtrl(depthWarp, softThread)).Lit(
          _.regIndex -> count.U,
          _.vecMask -> 0.U,
          _.warpID -> 0.U,
          _.wvd -> false.B,
          _.wxd -> false.B
        )
      )
    }
  }
  object vecFPUModuleInput { d: vecFPUInput[TestFPUCtrl] =>
    var count = 0
    val defaultMask = 1<<softThread - 1
    def reset = { count = 0 }
    def apply(a: Array[AnyVal], b: Array[AnyVal], c: Array[AnyVal], op: UInt, mask: BigInt = defaultMask): vecFPUInput[TestFPUCtrl] = {
      count = (count + 1) % 32
      (new vecFPUInput(softThread, len, new TestFPUCtrl(depthWarp, softThread))).Lit(
        _.data -> Vec(softThread, new FPUInput(len, EmptyFPUCtrl(), true)).Lit((0 until softThread).map { i =>
          i -> new FPUInput(len, EmptyFPUCtrl(), true).Lit(
            _.a -> toUInt(a(i)).U,
            _.b -> toUInt(b(i)).U,
            _.c -> toUInt(c(i)).U,
            _.op -> op,
            _.rm -> RTZ
          )
        }:_*),
        _.ctrl -> (new TestFPUCtrl(depthWarp, softThread)).Lit(
          _.regIndex -> count.U,
          _.vecMask -> mask.U,
          _.warpID -> 0.U,
          _.wvd -> false.B,
          _.wxd -> false.B
        )
      )
    }
  }

  behavior of "FPU"
  it should "FPU Operations" in {
    test(new ScalarFPU(expWidth, precision, new TestFPUCtrl(depthWarp, softThread))).withAnnotations(Seq(WriteVcdAnnotation)) { d =>
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
    test(new VectorFPU(expWidth, precision, softThread, hardThread, new TestFPUCtrl(depthWarp, softThread))).withAnnotations(Seq(WriteVcdAnnotation)){ d =>
      vecFPUModuleInput.reset
      d.io.in.initSource()
      d.io.in.setSourceClock(d.clock)
      d.io.out.initSink()
      d.io.out.setSinkClock(d.clock)
      d.clock.setTimeout(20)
      d.io.out.ready.poke(true.B)
      d.io.in.valid.poke(false.B)
      d.clock.step(5)
      fork{
        d.io.in.enqueueSeq(Seq(
          vecFPUModuleInput((0 until softThread).map{100f + _}.toArray, Seq.fill(softThread)(1f).toArray, Seq.fill(softThread)(-100f).toArray, FN_FMADD),
          vecFPUModuleInput((0 until softThread).map{200f + _}.toArray, Seq.fill(softThread)(0).toArray, Seq.fill(softThread)(0).toArray, FN_F2I),
          vecFPUModuleInput((0 until softThread).map{300f + _}.toArray, Seq.fill(softThread)(0).toArray, Seq.fill(softThread)(0).toArray, FN_F2I),
          vecFPUModuleInput((0 until softThread).map{400f + _}.toArray, Seq.fill(softThread)(0).toArray, Seq.fill(softThread)(0).toArray, FN_F2I),
          vecFPUModuleInput((0 until softThread).map{500f + _}.toArray, Seq.fill(softThread)(0).toArray, Seq.fill(softThread)(0).toArray, FN_F2I)
        ))
      }.fork {
        d.io.out.ready.poke(true.B)
      }.join()
      d.clock.step(10)
    }
  }
}
