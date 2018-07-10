package at.forsyte.harrsh.heapautomata.utils

import at.forsyte.harrsh.seplog.inductive.SymbolicHeap

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
    val padding = List.fill(sh.predCalls.length - newHeaps.length)(SymbolicHeap.empty)
    sh.replaceCalls(newHeaps ++ padding)
  }

}
