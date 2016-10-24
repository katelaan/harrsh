# HARRSH #

HARRSH implements Heap Automata for Reasoning about Robustness of Symbolic Heaps.
More specifically, for various *robustness properties*, HARRSH implements

 1. Automatic **refinement** of a given system of inductive definitions (SID) to filter out those unfoldings that do *not* satisfy the robustness property.
 2. **Decision procedures** for deciding whether a given SID has unfoldings that satisfy the robustness property.
 
Currently, the following robustness properties are supported for arbitrary SIDs.

* Satisfiability and unsatisfiability checking
* Establishment checking to prove absence of dangling pointers
* Reachability to show definite reachability between pairs of variables in the heap
* Garbage freedom, i.e., absence of unreachable allocated memory locations
* Weak acyclicity, i.e., acyclicity of all paths involving only non-dangling pointers

HARRSH is co-developed by [FORSYTE](http://forsyte.at) (TU Wien) and [MOVES](http://moves.rwth-aachen.de) (RWTH Aachen University).
For more details, see the preprint of our paper "Unified Reasoning about Robustness Properties of Symbolic-Heap Separation Logic" (by Christina Jansen, Jens Katelaan, Christoph Matheja, Thomas Noll, and Florian Zuleger) on [arXiv.org](http://arxiv.org).

### Installation ###

Just clone the repository and run "sbt compile" followed by "sbt run" in your working copy.
More detailed instructions are available in a separate INSTALL file.

### Tutorial 

#### Writing SID Specifications ####

HARRSH can read SID specifications both in the Cyclist format (`.defs` files) or in our own format. We now describe our own format.
  
      # SID files may contain comment lines introduced by #
      # SID files consist of a sequence of rules, separated by ;
      # The ; is mandatory. If you get a parse error, a likely cause is a missing ;
        
      # Rules are of the form <predicate symbol> <= <predicate body>, for example,
      # to define a predicate dummy that can be replaced by a pointer from x1 to x2
      foo <= x1 -> x2 ;
        
      # Multiple targets for pointers are possible
      foo <= x1 -> (x2, x3) ;
        
      # Free variables MUST be named x1,x2,x3...
      # All other variables are implicitly existentially quantified, e.g. the z in
      foo <= x1 -> (z, x2) ;
        
      # The null pointer can be written either as null or as nil
      foo <= x1 -> (null,nil) ;
        
      # The empty heap is written emp, multiple spatial atoms separated by *
      foo <= emp * x2 -> nil ;
        
      # Predicate calls are written as <pred name>(<arg>*)
      # You have to ensure yourself that you pass the correct number of arguments
      bar <= foo(x1, y, nil) ;
        
      # (In)equalities between pointers are written in a suffix : { <(in)equalities> },
      # using the syntax <ptr> = <ptr> or <ptr> != <ptr>, separated by commas
      bar <= x3 -> z * foo(x1, z, y) : { y = z, x1 != x2 } ;
        
      # Use as little or as much white space as you please, e.g.
      bar <= x3->(z,y,   x2)*foo(x1,z,y):{y=z, x1    !=  x2 }
        
      # That's it. By the way: This file should parse.
  

#### Properties ####

HARRSH currently supports checking (see decision procedures) and establishing (see refinement) the following properties for a given SID:

* SAT :  Does there exists a satisfiable unfolding?
* UNSAT :  Does there exist an unsatisfiable unfolding? Note that this is **not** the complement of SAT, as an SID can have both satisfiable and unsatisfiable unfoldings. 
* EST :  Does there exist an established unfolding, i.e., an unfolding in which all variables are either allocated or equal to a free variable? This is often a precondition for applying other separation logic decision procedures, for example for entailment checking.
* ACYC :      Does there exist a Weakly acyclic unfolding?
* GF :    Does there exist a garbage-free unfolding? 
* HASPTR :            Does there exist an Unfolding that allocates memory?
* NON-EST :           Does there exist a non-established unfolding?
* REACH(a,b) :        Does there exist an unfoldings where b is reachable from a, for a,b in {x1,x2,..,}?
* TRACK(a,b,...) :    Does there exist an unfoldingu in which free variables a,b,... are def. allocated

#### Executing Refinement ####

HARRSH implements a refinement algorithm that takes as input the path to an SID specification and a property in the format described above and returns a new refined SID in which all unfoldings that do **not** satisfy the property have been removed.

SID refinement has multiple applications:

* Debugging. E.g. finding unsatisfiable unfoldings of a data structure specification that should only have satisfiable unfoldings.
* Preprocessing. E.g. guaranteeing that all unfoldings of an SID are established prior to feeding it to an entailment checker. 
* Optimization. E.g. removing irrelevant unfoldings from the SID, thus narrowing down the search space for verification tools.

To execute SID refinement, run `sbt "run --refine <path/to/sid> --prop <property>"`, where property is in the format described above.
Once refinement is complete, the refined SID is printed to `stdout`.

**Try it out!** Run `sbt "run --refine examples/datastructures/tll.sid --prop REACH(x3,x2)"`. HARRSH will return a refined SID together with a warning that the refined SID does not have a rule for the `tll` predicate. This means that the refined SID is empty and thus that the original SID does *not* have an unfolding in which `x2` is reachable from `x3`.

    Will refine SID definition in file examples/datastructures/tll.sid by REACH(x3,x2)
    WARNING: Language of refined SID is empty (no rules for start predicate 'tll').
    Refinement of tll-SID with REACH_3 (start predicate 'tll'): 
        tll0 <= ∃l ∃r ∃z . x1 ↦ (l, r, null) * tll0(l,x2,z) * tll1(r,z,x3)
        tll0 <= ∃l ∃r ∃z . x1 ↦ (l, r, null) * tll1(l,x2,z) * tll1(r,z,x3)
        tll0 <= ∃l ∃r ∃z . x1 ↦ (l, r, null) * tll1(l,x2,z) * tll0(r,z,x3)
        tll0 <= ∃l ∃r ∃z . x1 ↦ (l, r, null) * tll0(l,x2,z) * tll0(r,z,x3)
        tll1 <= x1 ↦ (null, null, x3) : {x1 ≈ x2}

#### Executing Decision Procedures ####

The refinement algorithm can also be used to decide whether there exist unfoldings of a given SID that have a property of interest (such as satisfiability, acyclicity, etc.). The decision procedures generally outperform the explicit refinement, as they perform on-the-fly refinement (Algorithm 1 in the paper).

Currently, HARRSH only processes decision problem instances in batch mode. To do so, create a benchmark file with tasks to perform (see the example folder) and feed it to HARRSH: `sbt "run --batch <path/to/tasks> --timeout <timeout in seconds>"`. The timeout is optional (120 seconds by default).

**Try it out!** Run `sbt "run --batch examples/basic-benchmarks.bms"`. HARRSH will check various robustness properties for various simple data structure specifications and summarize the results in a table.

### Who do I talk to? ###

If you have questions, would like more information about the implementation or report a bug, don't hesitate to contact [Jens Katelaan](mailto:jkatelaan@forsyte.at).