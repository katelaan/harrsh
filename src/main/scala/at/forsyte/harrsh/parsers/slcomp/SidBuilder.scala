package at.forsyte.harrsh.parsers.slcomp

import at.forsyte.harrsh.main.HarrshLogging
import at.forsyte.harrsh.seplog.inductive.{SID, SymbolicHeap}

sealed trait SidBuilder {

  def aggregate(other: SidBuilder): SidBuilder = (this,other) match {
    case (Dummy, o) =>
      //println(s"Retaining 2nd arg $o")
      o
    case (self, Dummy) =>
      //println(s"Retaining 1st arg $self")
      self
    case (kw: Keyword, av: AttributeValue) => Attribute(kw, av)
    case (Args(args), arg) => Args(args :+ arg)
    case (node, otherNode) =>
      Args(List(node, otherNode))
  }

  def isIdentifier: Boolean = this match {
    case s: Symbol => true
    case i: IndexedIdentifier => true
    case _ => false
  }

  def isConstantValue: Boolean = this match {
    case d: Decimal => true
    case s: StrVal => true
    case _ => false
  }

  def toList: List[SidBuilder] = this match {
    case Args(args) => args
    case _ => throw new NotImplementedError(s"Can't convert $this to list")
  }

}

/* Auxiliary classes */
case object Dummy extends SidBuilder
case class CmdTypeWrapper(ct: CommandType) extends SidBuilder
case class Args(args: List[SidBuilder]) extends SidBuilder

/* Constant data */
case class Decimal(d: Double) extends SidBuilder
case class Numeral(i: Int) extends SidBuilder
case class StrVal(s: String) extends SidBuilder
case class Constant(c: SidBuilder) extends SidBuilder {
  assert(c.isConstantValue)
}

/* Symbols, identifiers, and attributes */
case class Symbol(str: String) extends SidBuilder {
  def isQuoted: Boolean = str.charAt(0) == '|'
}
case class IndexedIdentifier(s: Symbol, ixs: List[SidBuilder]) extends SidBuilder {
  for (ix <- ixs)
    assert(ix.isInstanceOf[Numeral] || ix.isInstanceOf[Symbol])
}
case class QualifiedIdentifier(id: SidBuilder, sort: Sort) extends SidBuilder {
  assert(id.isIdentifier)
}

case class Keyword(s: String) extends SidBuilder
case class AttributeValue(v: SidBuilder) extends SidBuilder {
  assert(v.isInstanceOf[StrVal] || v.isInstanceOf[Constant] || v.isInstanceOf[Symbol])
}

case class Attribute(kw: Keyword, av: AttributeValue) extends SidBuilder

/* Sorts */
case class SortDecl(name: Symbol, arity: Numeral) extends SidBuilder
case class SortedVar(name: Symbol, sort: Sort) extends SidBuilder

/* Datatypes */
case class Sort(symbol: Symbol) extends SidBuilder
case class Selector(sel: Symbol, sort: Sort) extends SidBuilder
case class ConstructorDecl(name: Symbol, sels: List[Selector], params: Option[List[Symbol]]) extends SidBuilder
case class DataTypeDecl(sort: SortDecl, constructors: List[ConstructorDecl]) extends SidBuilder
case class DataTypes(ds: List[DataTypeDecl]) extends SidBuilder {

  def get(typeName: String): Option[DataTypeDecl] = {
    ds.find(_.sort.name.str == typeName)
  }

  def getConstructor(consName: String): ConstructorDecl = {
    val cons : List[ConstructorDecl] = ds.flatMap(_.constructors.find(_.name.str == consName))
    cons.head
  }

}

/* Heap */
case class HeapDecl(mapping: List[(Sort,Sort)]) extends SidBuilder

/* Constants and Functions */
case class ConstDecl(name: Symbol, sort: Sort) extends SidBuilder
case class FunDecl(name: Symbol, args: List[SortedVar], ret: Sort) extends SidBuilder
case class FunDef(decl: FunDecl, term: SidBuilder) extends SidBuilder
case class FunDefs(defs: List[FunDef]) extends SidBuilder

/* Terms */
case class Exists(vars: List[SortedVar], term: SidBuilder) extends SidBuilder

/* Solver commands */
case class Assert(term: SidBuilder) extends SidBuilder
case class Task(task: Task.Type, args: List[SidBuilder]) extends SidBuilder

object Task {
  sealed trait Type
  case object CheckSat extends Type
  case object CheckUnsat extends Type
  case object GetModel extends Type
}

/* Meta data */
case class Meta(metaType: String, content: List[SidBuilder]) extends SidBuilder

object Meta {

  object Type {
    val SetLogic = "set-logic"
    val SetInfo = "set-info"
  }
}

/* Overall script */
case class Script(sorts: List[SortDecl],
                  types: Option[DataTypes],
                  heap: HeapDecl,
                  consts: List[ConstDecl],
                  funs: List[FunDef],
                  asserts: List[Assert],
                  meta: List[Meta],
                  tasks: List[Task]) extends SidBuilder {

  def toSid : SID = ScriptToSid(this)

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append("Script { \n")

    if (sorts.nonEmpty) {
      sb.append("  sorts = {\n")
      sb.append(sorts.map(c => s"    $c").mkString(",\n"))
      sb.append("  }\n")
    }

    if (types.isDefined) {
      sb.append("  data_types = {\n")
      sb.append(types.get.ds.map(c => s"    $c").mkString(",\n"))
      sb.append("  }\n")
    }

    sb.append("  heap_decl = {\n")
    sb.append(s"    $heap\n")
    sb.append("  }\n")

    sb.append("  consts = {\n")
    sb.append(consts.map(c => s"    $c").mkString(",\n"))
    sb.append("  }\n")

    sb.append("  funs = {\n")
    sb.append(funs.map(c => s"    $c").mkString(",\n"))
    sb.append("  }\n")

    sb.append("  asserts = {\n")
    sb.append(asserts.map(c => s"    $c").mkString(",\n"))
    sb.append("  }\n")

    sb.append("  meta = {\n")
    sb.append(meta.map(c => s"    $c").mkString(",\n"))
    sb.append("  }\n")

    sb.append("  tasks = {\n")
    sb.append(tasks.map(c => s"    $c").mkString(",\n"))
    sb.append("  }\n")

    sb.append("}")
    sb.mkString
  }

}

sealed trait CommandType extends HarrshLogging {

  def assembleCmd(args: List[SidBuilder]): SidBuilder

}

trait NotSupportedCommand {
  val cmdName: String

  def assembleCmd(args: List[SidBuilder]): SidBuilder = {
    throw new NotImplementedError(s"The command $cmdName is not supported.")
  }
}

case object CmdAssert extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = {
    assert(args.length == 1)
    Assert(args(0))
  }
}

case object CmdCheckSat extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = Task(Task.CheckSat, Nil)
}

case object CmdCheckSatAssuming extends CommandType with NotSupportedCommand {
  override val cmdName = "check-sat-assuming"
}

case object CmdCheckUnsat extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = Task(Task.CheckUnsat, Nil)
}

case object CmdDeclareConst extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = {
    assert(args.length == 2)
    ConstDecl(args(0).asInstanceOf[Symbol], args(1).asInstanceOf[Sort])
  }
}

case object CmdDeclareFun extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = ???
}

case object CmdDeclareDatatype extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = CmdDeclareDatatypes.assembleCmd(
    // TODO: Try this
    List(SortDecl(args(0).asInstanceOf[Symbol], Numeral(0)), args(1))
  )
}

case object CmdDeclareDatatypes extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = {
    logger.debug(s"Constructing data types from ${args}")

    val (decls, terms) = args.span(_.isInstanceOf[SortDecl])
    assert(decls.length == terms.length)
    val defs = (decls, terms).zipped.map((d,t) => DataTypeDecl(d.asInstanceOf[SortDecl], List(t.asInstanceOf[ConstructorDecl])))
    val res = DataTypes(defs)
    logger.debug(s"Built data types $res")
    res
    //DataTypes(List(DataTypeDecl(args.head.asInstanceOf[SortDecl], args.tail.map(_.asInstanceOf[ConstructorDecl]))))
  }
}

case object CmdDeclareHeap extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = {
    logger.debug(s"Constructing heap declaration from $args")
    val mapping = for {
      Args(argv) <- args
    } yield (argv(0).asInstanceOf[Sort], argv(1).asInstanceOf[Sort])
    HeapDecl(mapping)
  }
}

case object CmdDeclareSort extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = (args(0), args(1)) match {
    case (s:Symbol, i:Numeral) => SortDecl(s, i)
    case _ => throw new Exception(s"Can't build sort decl from $args")
  }
}

case object CmdDefineFun extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = ???
}

case object CmdDefineFunRec extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = {
    assert(args.length == 1)
    assert(args(0).isInstanceOf[FunDef])
    args(0)
  }
}

case object CmdDefineFunsRec extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = {
//    for (arg <- args)
//      println(arg)
    val (decls, terms) = args.span(_.isInstanceOf[FunDecl])
    assert(decls.length == terms.length)
    val defs = (decls, terms).zipped.map((d,t) => FunDef(d.asInstanceOf[FunDecl], t))
    val res = FunDefs(defs)
//    println(res)
    res
  }
}

case object CmdDefineSort extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = ???
}

case object CmdEcho extends CommandType with NotSupportedCommand {
  override val cmdName = "echo"
}

case object CmdExit extends CommandType with NotSupportedCommand {
  override val cmdName = "exit"
}

case object CmdGetAssertions extends CommandType with NotSupportedCommand {
  override val cmdName = "get-assertions"
}

case object CmdGetAssignment extends CommandType with NotSupportedCommand {
  override val cmdName = "get-assignment"
}

case object CmdGetInfo extends CommandType with NotSupportedCommand {
  override val cmdName = "get-info"
}

case object CmdGetModel extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = Task(Task.GetModel, Nil)
}

case object CmdGetOption extends CommandType with NotSupportedCommand {
  override val cmdName = "get-option"
}

case object CmdGetProof extends CommandType with NotSupportedCommand {
  override val cmdName = "get-proof"
}

case object CmdGetUnsatAssumptions extends CommandType with NotSupportedCommand {
  override val cmdName = "get-unsat-assumptions"
}

case object CmdGetUnsatCore extends CommandType with NotSupportedCommand {
  override val cmdName = "get-unsat-core"
}

case object CmdGetValue extends CommandType with NotSupportedCommand {
  override val cmdName = "get-value"
}

case object CmdPop extends CommandType with NotSupportedCommand {
  override val cmdName = "pop"
}

case object CmdPush extends CommandType with NotSupportedCommand {
  override val cmdName = "push"
}

case object CmdReset extends CommandType with NotSupportedCommand {
  override val cmdName = "reset"
}

case object CmdResetAssertions extends CommandType with NotSupportedCommand {
  override val cmdName = "reset-assertions"
}

case object CmdSetLogic extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = {
    val res = Meta(Meta.Type.SetLogic, args)
    res
  }
}

case object CmdSetInfo extends CommandType {
  override def assembleCmd(args: List[SidBuilder]): SidBuilder = {
    val res = Meta(Meta.Type.SetInfo, args)
    res
  }
}

case object CmdSetOption extends CommandType with NotSupportedCommand {
  val cmdName = "set-option"
}