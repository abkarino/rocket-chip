package freechips.rocketchip.tile


import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.rocket.Instructions._
import chisel3.experimental._

case class VUParams(
  vLen: Int = 128
)

object VConstants
{
}
import VConstants._
trait HasVUCtrlSigs {

}

class VUCtrlSigs extends Bundle with HasVUCtrlSigs

class VUDecoder(implicit p: Parameters) extends VUModule()(p) {
  val io = new Bundle {
    val inst = Bits(INPUT, 32)
    val sigs = new VUCtrlSigs().asOutput
  }
}

class VUCoreIO(implicit p: Parameters) extends CoreBundle()(p) {
  val inst = Bits(INPUT, 32)
}

//TODO: Import the rest of the fpu functions

// COPY START
class VConfig(implicit p: Parameters) extends CoreBundle {
  val vl = UInt((maxVLMax.log2 + 1).W)
  val vtype = new VType
}

object VType {
  private def fromUInt(that: UInt, ignore_vill: Boolean)(implicit p: Parameters): VType = {
    val res = 0.U.asTypeOf(new VType)
    val in = that.asTypeOf(res)
    res.vill := (in.max_vsew < in.vsew) || in.reserved =/= 0 || in.vill
    when (!res.vill || ignore_vill) {
      res.vsew := in.vsew(log2Ceil(1 + in.max_vsew) - 1, 0)
      res.vlmul := in.vlmul
    }
    res
  }

  def fromUInt(that: UInt)(implicit p: Parameters): VType = fromUInt(that, false)

  def computeVL(avl: UInt, vtype: UInt, currentVL: UInt, useCurrentVL: Bool, useZero: Bool)(implicit p: Parameters): UInt =
    VType.fromUInt(vtype, true).vl(avl, currentVL, useCurrentVL, useZero)
}

class VType(implicit p: Parameters) extends CoreBundle {
  val vill = Bool()
  val reserved = UInt((xLen - 6).W)
  val vsew = UInt(3.W)
  val vlmul = UInt(2.W)

  val max_vsew = log2Ceil(eLen/8)

  def minVLMax = maxVLMax / eLen
  def vlMax: UInt = (maxVLMax >> (this.vsew +& ~this.vlmul)).andNot(minVLMax-1)
  def vlMaxInBytes: UInt = maxVLMax >> ~this.vlmul

  def vl(avl: UInt, currentVL: UInt, useCurrentVL: Bool, useZero: Bool): UInt = {
    val atLeastMaxVLMax = Mux(useCurrentVL, currentVL >= maxVLMax, avl >= maxVLMax)
    val avl_lsbs = Mux(useCurrentVL, currentVL, avl)(maxVLMax.log2 - 1, 0)

    val atLeastVLMax = atLeastMaxVLMax || (avl_lsbs & (-maxVLMax.S >> (this.vsew +& ~this.vlmul)).asUInt.andNot(minVLMax-1)).orR
    val isZero = vill || useZero
    Mux(!isZero && atLeastVLMax, vlMax, 0.U) | Mux(!isZero && !atLeastVLMax, avl_lsbs, 0.U)
  }
}
// COPY END
trait HasVUParameters {

}

abstract class VUModule(implicit p: Parameters) extends CoreModule()(p) with HasVUParameters

@chiselName
class VU(cfg: VUParams)(implicit p: Parameters) extends VUModule()(p) {
  val io = new Bundle {
  }

}
