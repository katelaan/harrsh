package at.forsyte.harrsh.heapautomata.utils

import at.forsyte.harrsh.seplog.Var
import at.forsyte.harrsh.seplog.inductive.{PredCall, SymbolicHeap}

/**
  * Created by jens on 3/29/17.
  */
trait Kernelizable {

  def kernel : SymbolicHeap

}

object Kernelizable {

  def compressByKernelization(sh : SymbolicHeap, qs : Seq[Kernelizable]) : SymbolicHeap = {
    val newHeaps = qs map (_.kernel)
    sh.replaceCalls(newHeaps)
  }

  def compressByPartialKernelization(sh : SymbolicHeap, qs : Seq[Kernelizable]) : SymbolicHeap = {
    val newHeaps = qs map (_.kernel)
    //val padding = List.fill(sh.predCalls.length - newHeaps.length)(SymbolicHeap.empty)
    val callsReplacedByEmptyHeaps: Seq[PredCall] = sh.predCalls.drop(qs.length)
    val padding = callsReplacedByEmptyHeaps map (call => SymbolicHeap(Seq.empty, Seq.empty, Seq.empty, Var.getFvSeq(call.args.length)))
    sh.replaceCalls(newHeaps ++ padding)
  }

}
