package FPUv2

object TestArgs {
  val expWidth = 8
  val precision = 24
  val len = expWidth + precision
  def toUInt[T <: AnyVal](f: T): String = {
    if(f.getClass == classOf[java.lang.Float]) {
      "h" + java.lang.Float.floatToIntBits(f.asInstanceOf[Float]).toHexString
    }
    else if(f.getClass == classOf[java.lang.Integer]) {
      "h" + java.lang.Integer.toHexString(f.asInstanceOf[Int])
    } else {
      "h0"
    }
  }
}
