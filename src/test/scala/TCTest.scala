package FPUv2

import FPUv2.utils.{TestFPUCtrl, FPUInput, RoundingModes}
import FPUv2.{TCDotProduct}
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals.AddVecLiteralConstructor
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TCDotProdTest extends AnyFlatSpec with ChiselScalatestTester {
  import TestArgs._
  object TCDotProdInput {
    var count = 0
    def reset = { count = 0 }
    def apply(DimN: Int, a: Seq[AnyVal], b: Seq[AnyVal], c: AnyVal) = {
      count = (count + 1) % 32
      new TCDotProductInput(DimN, 32, new TCCtrl(32, 1)).Lit(
        _.a -> Vec(DimN, UInt(32.W)).Lit(
          (0 until DimN).map{ i => i -> toUInt(a(i)).U}:_*
        ),
        _.b -> Vec(DimN, UInt(32.W)).Lit(
          (0 until DimN).map{ i => i -> toUInt(b(i)).U}:_*
        ),
        _.c -> toUInt(c).U,
        _.rm -> RoundingModes.RNE,
        _.ctrl.get -> new TCCtrl(32, 1).Lit(
          _.reg_idxw -> count.U,
          _.warpID -> 0.U
        )
      )
    }
  }

  behavior of "Tensor"
  it should "DotProduct" in {
    test(new TCDotProduct(4, 8, 24, new TCCtrl(32, 1))).withAnnotations(Seq(WriteVcdAnnotation)) { d =>
      TCDotProdInput.reset
      d.io.in.initSource()
      d.io.in.setSourceClock(d.clock)
      d.io.out.initSink()
      d.io.out.setSinkClock(d.clock)
      d.clock.setTimeout(20)
      d.io.out.ready.poke(true.B)
      fork{
        d.io.in.enqueueSeq(Seq(
          TCDotProdInput(4, (0 until 4).map(_.toFloat + 0f), (0 until 4).map(_.toFloat + 0f), 1000f),
          TCDotProdInput(4, (0 until 4).map(_.toFloat + 4f), (0 until 4).map(_.toFloat + 4f), 1000f)
        ))
      }.fork {
        d.io.out.ready.poke(true.B)
      }.join()
      d.clock.step(10)
    }
  }

//  object TCInput {
//    var count = 0
//
//    def reset = {
//      count = 0
//    }
//
//    def apply(vl: Int, Dim: Tuple3[Int, Int, Int], a: Seq[AnyVal], b: Seq[AnyVal], c: AnyVal) = {
//      count = (count + 1) % 32
//      new TensorCoreInput(, 32, new TCCtrl(32, 1)).Lit(
//        _.data ->
//      )
//    }
//  }
}

