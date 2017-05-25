package at.forsyte.harrsh.entailment

/**
  * Created by jkatelaa on 5/23/17.
  */
object ObservationTableClosure {

  // TODO Remove this object, handled by symmetry traits etc?

//  def closureForLearningMode(learningMode : EntailmentAutomatonLearning.LearningMode, numFV : Int, obs : ObservationTable) : ObservationTable = {
//    if (learningMode.closedUnderParameterRenaming) obs else closeUnderParameterRenaming(obs)
//    //val afterRenaming =
//    //if (learningMode.closedUnderEmp) afterRenaming else closeUnderEmp(numFV, obs)
//  }

//  def closeUnderEmp(numFV : Int, obs : ObservationTable) : ObservationTable = {
//
//    // Compute all the candidates we skipped before
//    val undeterminedRelationships : Seq[(Var,Var)] = for {
//      l <- 0 until numFV
//      r <- l+1 to numFV
//    } yield (Var(l), Var(r))
//
//    val empRshs = Determinization.determineRelationshipsOf(SymbolicHeap.empty, undeterminedRelationships)
//
//    // Check for each of these whether there is just one (non-emp) equivalence class in obs with the same extension behavior
//    // If so, merge it with that class; if not, make it a new class
//    empRshs.foldLeft(obs){
//      case (intermediateObs, empRsh) => addEmpHeapToObservationTable(intermediateObs, empRsh)
//    }
//  }
//
//  private def addEmpHeapToObservationTable(obs : ObservationTable, empRsh : SymbolicHeap) : ObservationTable = {
//    val entries = obs.getAllMatchingEntries(empRsh)
//    logger.warn("Matching entries for " + empRsh + ": " + entries.mkString(", "))
//    entries.size match {
//      case 0 =>
//        // Genuinely new entry
//        ???
//      case 1 =>
//        // Old entry
//        ???
//      case i =>
//        // More than one matching entry => emp behaves in a symmetrical fashion
//        ???
//    }
//  }

}
