# HARRSH #

HARRSH implements Heap Automata for Reasoning about Robustness of Symbolic Heaps.

## News: Entailment Checking with HARRSH

HARRSH now supports entailment checking. The theory behind HARRSH's entailment checker is presented in the TACAS 2019 paper "Effective Entailment Checking for Separation Logic with Inductive Definitions" (by Jens Katelaan, Christoph Matheja, and Florian Zuleger). The [appendix with all proofs](https://github.com/katelaan/entailment/blob/master/appendix.pdf) is available [here](https://github.com/katelaan/entailment/blob/master/appendix.pdf). An [archive containing the full set of benchmarks](https://github.com/katelaan/entailment/blob/master/benchmarks.tar.gz) used to evaluate Harrsh are available [here](https://github.com/katelaan/entailment/blob/master/benchmarks.tar.gz).
The archive contains the benchmarks in Harrsh, Songbird and Slide input formats.

## Robustness Properties

For various *robustness properties*, HARRSH implements

 1. Automatic **refinement** of a given system of inductive definitions (SID) to filter out those unfoldings that do *not* satisfy the robustness property.
 2. **Decision procedures** for deciding whether a given SID has unfoldings that satisfy the robustness property.
 
Currently, the following robustness properties are supported for arbitrary SIDs.

* Satisfiability and unsatisfiability checking
* Establishment checking to prove absence of dangling pointers
* Reachability to show definite reachability between pairs of variables in the heap
* Garbage freedom, i.e., absence of unreachable allocated memory locations
* Weak acyclicity, i.e., acyclicity of all paths involving only non-dangling pointers

Additionally, we now have support for **entailment** checking for the symbolic-heap fragment defined in [The Tree Width of Separation Logic with Recursive Definitions](https://link.springer.com/chapter/10.1007/978-3-642-38574-2_2)

HARRSH is co-developed by [FORSYTE](http://forsyte.at) (TU Wien) and [MOVES](http://moves.rwth-aachen.de) (RWTH Aachen University).
For more details, see the preprint of our paper ["Unified Reasoning about Robustness Properties of Symbolic-Heap Separation Logic"](https://arxiv.org/abs/1610.07041) (by Christina Jansen, Jens Katelaan, Christoph Matheja, Thomas Noll, and Florian Zuleger) on [arXiv.org](https://arxiv.org/abs/1610.07041).

### Installation ###

Just install [sbt](http://www.scala-sbt.org/), clone the repository and run `./build.sh` in your working copy. This will build an executable called `harrsh`. More detailed instructions are available in a separate INSTALL file.

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
        
      # Both null and nil represent the null pointer
      foo <= x1 -> (null,nil) ;
        
      # The empty heap is written emp, multiple spatial atoms separated by *
      foo <= emp * x2 -> nil ;
        
      # Predicate calls are denoted <pred name>(<arg>*)
      # You have to ensure yourself that you pass the correct number of arguments
      bar <= foo(x1, y, nil) ;
        
      # (In)equalities between pointers are added in a suffix : { <(in)equalities> },
      # using the syntax <ptr> = <ptr> or <ptr> != <ptr>, separated by commas
      bar <= x3 -> z * foo(x1, z, y) : { y = z, x1 != x2 } ;
        
      # Use as little or as much white space as you like, e.g.
      bar <= x3->(z,y,   x2)*foo(x1,z,y):{y=z, x1    !=  x2 }
        
      # That's it. By the way: This file should parse.

#### Entailment Checking ####

To use HARRSH for entailment checking, an SID definition has to be combined with a query. For example,

    # Every list of even length is a list
    query {
      even(x1, x2) |= sll(x1, x2)
    }
    sid {
      sll <= x1 -> x2 ;
      sll <= x1 -> y * sll(y,x2) ;
      odd <= x1 -> x2 ;
      odd <= x1 -> y * even(y,x2) ;
      even <= x1 -> y * odd(y,x2)
    }
    info {
      status = true
    }

To check this entailment

**Try it out!** To check the above entailment, run `./harrsh -e examples/entailment/lists_singly_linked/even-sll_sll.hrs
`.

#### Properties ####

HARRSH currently supports checking (see decision procedures) and establishing (see refinement) the following properties for a given SID:

* SAT :  Does there exist a satisfiable unfolding?
* UNSAT :  Does there exist an unsatisfiable unfolding? Note that this is **not** the complement of SAT, as a predicate can have both satisfiable and unsatisfiable unfoldings. 
* EST :  Does there exist an established unfolding, i.e., an unfolding in which all variables are either allocated or equal to a free variable? This is often a precondition for applying other separation logic decision procedures, for example for entailment checking.
* ACYC :      Does there exist a weakly acyclic unfolding?
* GF :    Does there exist a garbage-free unfolding? 
* NON-EST, CYC, GARB: Complement of the above automata
* HASPTR :            Does there exist an unfolding that allocates memory?
* MOD[n,d] :           Does there exist an unfolding with n % d pointers?
* ALLOC[a,b,...] :        Does there exist an unfoldings where a,b,... are definitely allocated, for a,b in {x1,x2,..,}?
* REACH[a,b] :        Does there exist an unfoldings where b is reachable from a, for a,b in {x1,x2,..,}?
* TRACK[a,b,... : c~d,e~f] :    Does there exist an unfolding in which free variables a,b,... are def. allocated and the (in)equalities c~d,e~f,... def. hold, for ~ in {=,!=}

#### Executing Refinement ####

HARRSH implements a refinement algorithm that takes as input the path to an SID specification and a property in the format described above and returns a new refined SID in which all unfoldings that do **not** satisfy the property have been removed.

SID refinement has multiple applications:

* Debugging. E.g. finding unsatisfiable unfoldings of a data structure specification that should only have satisfiable unfoldings.
* Preprocessing. E.g. guaranteeing that all unfoldings of an SID are established prior to feeding it to an entailment checker. 
* Optimization. E.g. removing irrelevant unfoldings from the SID, thus narrowing down the search space for verification tools.

To execute SID refinement, run `./harrsh --refine <path/to/sid> --prop <property>`, where property is in the format described above.
Once refinement is complete, the refined SID is printed to `stdout`.

**Try it out!** Run `./harrsh --refine examples/datastructures/tll.sid --prop REACH[x3,x2]`. The file `tll.sid` defines trees with linked leaves with root `x1`, leftmost leaf `x2` and successor of rightmost leaf `x3`:

    tll <= x1 -> (nil, nil, x3) : { x1 = x2 } ;
    tll <= x1 -> (l, r, nil) * tll(l, x2, z) * tll(r, z, x3)

There is no unfolding of this SID in which `x2` is guaranteed to be reachable from `x3`.
HARRSH will therefore return a refined SID together with a warning that the refined SID does not have a rule for the `tll` predicate. This means that the refined SID is empty and thus that the original SID does *not* have an unfolding in which `x2` is reachable from `x3`.

    Will refine SID definition in file examples/datastructures/tll.sid by REACH[x3,x2]
    WARNING: Language of refined SID is empty (no rules for start predicate 'tll').
    Refinement of tll-SID with REACH_3 (start predicate 'tll'): 
        tll0 <= ∃l ∃r ∃z . x1 ↦ (l, r, null) * tll0(l,x2,z) * tll1(r,z,x3)
        tll0 <= ∃l ∃r ∃z . x1 ↦ (l, r, null) * tll1(l,x2,z) * tll1(r,z,x3)
        tll0 <= ∃l ∃r ∃z . x1 ↦ (l, r, null) * tll1(l,x2,z) * tll0(r,z,x3)
        tll0 <= ∃l ∃r ∃z . x1 ↦ (l, r, null) * tll0(l,x2,z) * tll0(r,z,x3)
        tll1 <= x1 ↦ (null, null, x3) : {x1 ≈ x2}

#### Executing Decision Procedures ####

The refinement algorithm can also be used to decide whether there exist unfoldings of a given SID that have a property of interest (such as satisfiability, acyclicity, etc.). The decision procedures generally outperform the explicit refinement, as they perform on-the-fly refinement (Algorithm 1 in the paper).

To run a single decision problem instance, run `./harrsh.sh --decide <path/to/sid> --prop <property>`.
HARRSH can also run multiple decision problem instances in batch mode. To do so, create a benchmark file with tasks to perform (see the example folder) and feed it to HARRSH: `./harrsh.sh --batch <path/to/tasks> --timeout <timeout in seconds>`. The timeout is optional (120 seconds by default).

**Try it out!** Run `./harrsh --batch examples/datastructure-benchmarks.bms`. HARRSH will check various robustness properties for various simple data structure specifications and summarize the results in a table.

### Who do I talk to? ###

If you have questions, would like more information about the implementation or report a bug, don't hesitate to contact [Jens Katelaan](mailto:jkatelaan@forsyte.at).
