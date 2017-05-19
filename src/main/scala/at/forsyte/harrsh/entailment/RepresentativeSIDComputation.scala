package at.forsyte.harrsh.entailment

import at.forsyte.harrsh.seplog.{PtrVar, Var}
import at.forsyte.harrsh.seplog.inductive.{PredCall, Rule, SID, SymbolicHeap}

/**
  * Created by jens on 5/3/17.
  */
object RepresentativeSIDComputation {

  def addBaseRule(repSid: SID, rep: SymbolicHeap) : SID = {

    val newBaseRule = Rule(
        head = repSid.startPred,
        freeVars = (1 to repSid.numFV).map(Var(_).toString),
        qvars = rep.boundVars.toSeq map (_.toString),
        body = rep
    )

    repSid.copy(rules = repSid.rules :+ newBaseRule)
  }

  def adaptSIDToRepresentative(sid : SID, rep : SymbolicHeap) : SID = {

    val tag = "^"
    val newStartPred = sid.startPred + tag
    val newNumFV = rep.numFV

    // Compute new recursive rules: In each rule, replace exactly one call with the new start pred
    val newRules = for {
      rule <- sid.rules
      call@PredCall(name, args) <- rule.body.predCalls
      // Add new arguments if the arity has increased
      // FIXME What to do if the arity decreases / in the case where we have null? Can that be deferred completely to postprocessing?
      newVars : Seq[Var] = (args.size+1 to newNumFV).map(Var(_))
      newCall = PredCall(newStartPred, args ++ newVars.map(PtrVar))
      newFreeVars  = rule.freeVars ++ newVars.map(_.toString)
    } yield rule.copy(head = newStartPred,
                      freeVars = newFreeVars,
                      body = rule.body.copy(predCalls = rule.body.predCalls.updated(rule.body.predCalls.indexOf(call), newCall)))

    // TODO Optimization for linear structures (drop all old rules)

    // Combine rules + set new start pred
    addBaseRule(sid.copy(startPred = newStartPred, numFV = newNumFV, rules = sid.rules ++ newRules), rep)
  }

}
