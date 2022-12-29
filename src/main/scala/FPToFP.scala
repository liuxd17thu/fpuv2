package FPUv2

import chisel3._
import chisel3.util._
import FPUv2.utils.FPUOps._
import FPUv2.utils.{EmptyFPUCtrl, FPUPipelineModule, FPUSubModule}
import fudian.FPToFP

class FPToFP(ctrlGen: Data = EmptyFPUCtrl()) extends FPUPipelineModule(64, ctrlGen){
  override def latency = 2

}
