package FPUv2.utils

import chisel3._
import chisel3.util._

trait HasPipelineReg {
  this: FPUSubModule =>
  def latency: Int

  //val ready = Wire(Bool())
  //val cnt = RegInit(0.U((log2Up(latency)+1).W))

  //ready := (cnt < latency.U) || (cnt === latency.U && io.out.ready)
  //cnt := cnt + io.in.fire() - io.out.fire()

  val valids = io.in.valid +: Array.fill(latency)(RegInit(false.B))
  for (i <- 1 to latency) {
    when(!(!io.out.ready && valids.drop(i).reduce(_ && _))) {
      valids(i) := valids(i - 1)
    }
  }

  def regEnable(i: Int): Bool = valids(i - 1) && !(!io.out.ready && valids.drop(i).reduce(_ && _))


  def PipelineReg[T <: Data](i: Int)(next: T) = RegEnable(next, valids(i - 1) && !(!io.out.ready && valids.drop(i).reduce(_ && _)))

  def S1Reg[T <: Data](next: T): T = PipelineReg[T](1)(next)

  def S2Reg[T <: Data](next: T): T = PipelineReg[T](2)(next)

  def S3Reg[T <: Data](next: T): T = PipelineReg[T](3)(next)

  def S4Reg[T <: Data](next: T): T = PipelineReg[T](4)(next)

  def S5Reg[T <: Data](next: T): T = PipelineReg[T](5)(next)

  io.in.ready := !(!io.out.ready && valids.drop(1).reduce(_ && _))
  io.out.valid := valids.last
}
