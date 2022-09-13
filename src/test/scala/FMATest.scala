package FPUv2

import FPUv2.utils.FPUOps._
import FPUv2.utils.RoundingModes._
import chisel3._
import chisel3.util._
import chisel3.tester._
import org.scalatest.{FreeSpec, color}
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation

class FMATest extends FreeSpec with ChiselScalatestTester {
  def floatToString(f: Float): String = {
    "0x" + java.lang.Float.floatToIntBits(f).toHexString
  }

  def subModuleInput(_a: Float, _b: Float, _c: Float, _op: UInt, _rm: UInt = RTZ,
                     hasCtrl: Boolean = false) = new Bundle {
    val a = floatToString(_a).U
    val b = floatToString(_b).U
    val c = floatToString(_c).U
    val op = _op.tail(3)
    val rm = _rm
    val ctrl = if (hasCtrl) Some(new FPUv2.utils.FPUCtrl) else None
  }

  "FMA Instr Test" in {
    test(new FMA(expWidth = 8, precision = 24, hasCtrl = false)).withAnnotations(Seq(WriteVcdAnnotation)) {
      d =>
        fork{
          d.io.in.enqueueSeq(Seq(
            subModuleInput(1.0f, 2.0f, 3.0f, FN_FMADD),
            subModuleInput(2.0f, 2.0f, 0.0f, FN_FMUL),
            subModuleInput(3.0f, 3.0f, 0.0f, FN_FADD)
          ))
        }.fork {
          d.io.out.ready.poke(true.B)
        }.join()
    }
  }
}
