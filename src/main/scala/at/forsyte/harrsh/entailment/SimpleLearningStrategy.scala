package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.inductive.{PointsTo, PredCall, PureAtom, SymbolicHeap}

/**
  * Created by jkatelaa on 5/25/17.
  */
trait SimpleLearningStrategy extends LearningStrategy {

  override def iterationPostprocessing(obs : ObservationTable) : ObservationTable = obs

  override def checkPartition(partition : SymbolicHeapPartition, obs : ObservationTable, it : Int, entailmentLog : EntailmentLearningLog) : ObservationTable = {

    def updateEntry(entry : TableEntry) = {
      if (entry.discoveredInIteration < it) {
        logger.debug("Don't add " + partition + ", because entry is already closed")
        obs
      } else {
        logger.debug("Matching entry " + entry + " is still open, will add " + partition)
        val (obsWithRep,entryWithRep) = obs.addRepresentativeToEntryAndReturnEntry(entry, partition.rep)
        enlargeEntryWithExtension(obsWithRep, entryWithRep, partition, it, entailmentLog)
      }
    }

    val entries = obs.getAllMatchingEntries(partition.rep)
    entries.size match {
      case 0 =>
        // No match, new class
        withNewTableEntryFromPartition(obs, partition, it)
      case 1 =>
        // One match => Extend the class (which might trigger a split though, if it actually represents more than one equivalence class!)
        updateEntry(entries.head)
      case i =>
        // Need to implement order on equivalence classes...
        logger.warn(i + " matching entries for " + partition + ":\n" + entries.mkString("\n"))
        val strongest = strongestEntry(entries)
        logger.warn("Strongest entry: " + strongest)
        updateEntry(strongest)
    }
  }

  private def strongestEntry(entries : Seq[TableEntry]) : TableEntry = {

    def isWeakerEntryIn(exts : Set[(Set[PointsTo], Set[PureAtom], PredCall)])(ext : (SymbolicHeap, PredCall)) : Boolean = {
      // FIXME This is a very naive check. Have to improve this as soon as we run into the exception thrown below
      val (ptrSet, pureSet) = (ext._1.pointers.toSet, ext._1.pure.toSet)
      val matchingExt = exts.find(triple => triple._1 == ptrSet && triple._3 == ext._2)
      matchingExt.exists{
        _._2 subsetOf pureSet
      }
    }

    def isStronger(fst : TableEntry, snd : TableEntry) = {
      val sndAsSets = snd.exts.map{
        case (ext,call) => (ext.pointers.toSet, ext.pure.toSet, call)
      }
      fst.exts forall isWeakerEntryIn(sndAsSets)
    }

    entries.tail.foldLeft(entries.head){
      case (fst, snd) =>
        if (isStronger(fst,snd)) fst
        else if (isStronger(snd,fst)) snd
        else throw new Throwable("Cannot resolve ambiguity in table entries")
    }
  }

}
