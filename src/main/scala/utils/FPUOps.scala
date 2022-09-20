package FPUv2.utils

import chisel3._
import chisel3.util._

object FPUOps {
  val SZ_FPU_FUNC = 6

  def FN_FX = BitPat("b??????")

  def FN_FADD = 0.U(6.W) // 000 000      io.(a + b)

  def FN_FSUB = 1.U(6.W) // 000 001      io.(a - b)

  def FN_FMUL = 2.U(6.W) // 000 010      io.(a * b)

  def FN_FMADD = 4.U(6.W) // 000 100      io.(a * b + c)

  def FN_FMSUB = 5.U(6.W) // 000 101      io.(a * b - c)

  def FN_FNMSUB = 6.U(6.W) // 000 110      io.(-a * b + c)

  def FN_FNMADD = 7.U(6.W) // 000 111      io.(-a * b - c)

  def FN_MIN = 8.U(6.W) // 001 000

  def FN_MAX = 9.U(6.W) // 001 001

  def FN_FLE = 10.U(6.W) // 001 010

  def FN_FLT = 11.U(6.W) // 001 011

  def FN_FEQ = 12.U(6.W) // 001 100

  def FN_FNE = 13.U(6.W) // 001 101

  def FN_FCLASS = 18.U(6.W) // 010 010

  def FN_FSGNJ = 22.U(6.W) // 010 110

  def FN_FSGNJN = 21.U(6.W) // 010 101

  def FN_FSGNJX = 20.U(6.W) // 010 100

  def FN_F2IU = 24.U(6.W) // 011 000

  def FN_F2I = 25.U(6.W) // 011 001

  def FN_F2LU = 26.U(6.W) // 011 010

  def FN_F2L = 27.U(6.W) // 011 011

  def FN_IU2F = 32.U(6.W) // 100 000

  def FN_I2F = 33.U(6.W) // 100 001

  def FN_LU2F = 34.U(6.W) // 100 010

  def FN_L2F = 35.U(6.W) // 100 011

  def isADDSUB(op: UInt): Bool = {
    if (op.getWidth == 3)
      op(2, 1) === 0.U
    else if (op.getWidth == 6)
      op(2, 1) === 0.U && op.head(3) === 0.U
    else
      false.B
  }

  def isFMA(op: UInt): Bool = {
    if (op.getWidth == 3)
      op(2) === 1.U
    else if (op.getWidth == 6)
      op(2) === 1.U && op.head(3) === 0.U
    else
      false.B
  }

  def isFMUL(op: UInt): Bool = {
    if (op.getWidth == 3)
      op === FN_FMUL(2, 0)
    else if (op.getWidth == 6)
      op === FN_FMUL
    else
      false.B
  }

  def withSUB(op: UInt): Bool = {
    op(0) === 1.U
  }

  def withInvProd(op: UInt): Bool = {
    if (op.getWidth == 3)
      op === FN_FNMADD(2, 0) || op === FN_FNMSUB
    else if (op.getWidth == 6)
      op === FN_FNMADD || op === FN_FNMSUB
    else
      false.B
  }

  def signedConvert(op: UInt): Bool = op(0).asBool

  def longConvert(op: UInt): Bool = op(1).asBool

  def doubleConvert(op: UInt): Bool = op(2).asBool
}

object RoundingModes {
  def RNE: UInt = 0.U(3.W)

  def RTZ: UInt = 1.U(3.W)

  def RDN: UInt = 2.U(3.W)

  def RUP: UInt = 3.U(3.W)

  def RMM: UInt = 4.U(3.W)
}