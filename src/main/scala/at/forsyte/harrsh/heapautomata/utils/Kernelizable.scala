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

}
