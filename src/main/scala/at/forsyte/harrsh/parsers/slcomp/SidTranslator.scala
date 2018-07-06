package at.forsyte.harrsh.parsers.slcomp

import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree._

class SidTranslator extends AbstractParseTreeVisitor[SidBuilder] with SLComp18Visitor[SidBuilder] {

  override def defaultResult(): SidBuilder = Dummy

  override def aggregateResult(aggregate: SidBuilder, nextResult: SidBuilder): SidBuilder = aggregate.aggregate(nextResult)

  def fail(ctx: ParserRuleContext): SidBuilder = {
    println(s"Text: ${ctx.getText}")
    println(s"Children: ${visitChildren(ctx)}")
    ???
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#start`.
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitStart(ctx: SLComp18Parser.StartContext): SidBuilder = {
    visitChildren(ctx)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitResponse(ctx: SLComp18Parser.ResponseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#generalReservedWord`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitGeneralReservedWord(ctx: SLComp18Parser.GeneralReservedWordContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#simpleSymbol`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitSimpleSymbol(ctx: SLComp18Parser.SimpleSymbolContext): SidBuilder = {
    Symbol(ctx.getText)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#quotedSymbol`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitQuotedSymbol(ctx: SLComp18Parser.QuotedSymbolContext): SidBuilder = {
    Symbol(ctx.getText)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#predefSymbol`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitPredefSymbol(ctx: SLComp18Parser.PredefSymbolContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#predefKeyword`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitPredefKeyword(ctx: SLComp18Parser.PredefKeywordContext): SidBuilder = {
    val res = Keyword(ctx.getText)
    res
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#symbol`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitSymbol(ctx: SLComp18Parser.SymbolContext): SidBuilder = {
    // Just propagate the simple/quoted symbol constructed from the children
    visitChildren(ctx)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#numeral`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitNumeral(ctx: SLComp18Parser.NumeralContext): SidBuilder = {
    Numeral(ctx.getText.toInt)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#decimal`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitDecimal(ctx: SLComp18Parser.DecimalContext): SidBuilder = {
    val res = Decimal(ctx.getText.toDouble)
    res
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#hexadecimal`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitHexadecimal(ctx: SLComp18Parser.HexadecimalContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#binary`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitBinary(ctx: SLComp18Parser.BinaryContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#string`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitString(ctx: SLComp18Parser.StringContext): SidBuilder = {
    StrVal(ctx.getText)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#keyword`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitKeyword(ctx: SLComp18Parser.KeywordContext): SidBuilder = {
    // Avoid another level of wrapping by propagating predefined keywords
    visitChildren(ctx)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#spec_constant`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitSpec_constant(ctx: SLComp18Parser.Spec_constantContext): SidBuilder = {
    Constant(visitChildren(ctx))
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#s_expr`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitS_expr(ctx: SLComp18Parser.S_exprContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#index`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitIndex(ctx: SLComp18Parser.IndexContext): SidBuilder = {
    // Just pass on the symbol/index rather than introducing another level of wrapping
    visitChildren(ctx)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#identifier`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitIdentifier(ctx: SLComp18Parser.IdentifierContext): SidBuilder = {
    visitChildren(ctx) match {
      case s:Symbol =>
        // TODO: Wrap in dedicated identifier type or not
        s
      case Args(args) =>
        IndexedIdentifier(args.head.asInstanceOf[Symbol], args.tail)
      case other =>
        throw new NotImplementedError(s"Can't construct identifier from $other")
    }
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#attribute_value`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitAttribute_value(ctx: SLComp18Parser.Attribute_valueContext): SidBuilder = {
//    println(s"Attribute Value ${ctx.getText}")
    val res = visitChildren(ctx) match {
      case s: Symbol =>
        AttributeValue(s)
      case c: Constant =>
        AttributeValue(c)
      case cres =>
        throw new Exception(s"Can't build attribute value from $cres")
    }
//    println(s"Returning $res")
    res
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#attribute`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitAttribute(ctx: SLComp18Parser.AttributeContext): SidBuilder = {
    // TODO: Construct Attribute directly here instead of in aggregation!
    visitChildren(ctx)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#sort`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitSort(ctx: SLComp18Parser.SortContext): SidBuilder = {
    visitChildren(ctx) match {
      case s: Symbol => Sort(s)
      case other => throw new Exception(s"Can't interpret $other as sort")
    }
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#qual_identifer`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitQual_identifer(ctx: SLComp18Parser.Qual_identiferContext): SidBuilder = {
    visitChildren(ctx) match {
      case s: Symbol => s
      case i: IndexedIdentifier => i
      case Args(args) =>
        assert(args.length == 2)
        QualifiedIdentifier(args(0).asInstanceOf[Symbol], args(1).asInstanceOf[Sort])
      case other =>
        throw new Exception(s"Can't build qualified identifier from $other")
    }
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#var_binding`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitVar_binding(ctx: SLComp18Parser.Var_bindingContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#sorted_var`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitSorted_var(ctx: SLComp18Parser.Sorted_varContext): SidBuilder = {
    val args = visitChildren(ctx).toList
    SortedVar(args(0).asInstanceOf[Symbol], args(1).asInstanceOf[Sort])
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#pattern`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitPattern(ctx: SLComp18Parser.PatternContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#match_case`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitMatch_case(ctx: SLComp18Parser.Match_caseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#term`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitTerm(ctx: SLComp18Parser.TermContext): SidBuilder = {
    //println(s"Matching term ${ctx.getText} w/ ${ctx.getChild(1)}")

    val child = ctx.getChild(1)
    if (child != null) {
      child.getText match {
        case "exists" =>
          //println("EXISTS!!")
          val Args(children) = visitChildren(ctx)
          val vars = children.init.map(_.asInstanceOf[SortedVar])
          val term = children.last
          //println(s"EXISTS RES: $vars")
          //println(s"EXISTS RES: $term")
          Exists(vars, term)
        case "forall" =>
          ???
        case "!" =>
          ???
        case "match" =>
          ???
        case other =>
          visitChildren(ctx)
      }
    } else {
      visitChildren(ctx)
    }
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#sort_symbol_decl`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitSort_symbol_decl(ctx: SLComp18Parser.Sort_symbol_declContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#meta_spec_constant`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitMeta_spec_constant(ctx: SLComp18Parser.Meta_spec_constantContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#fun_symbol_decl`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitFun_symbol_decl(ctx: SLComp18Parser.Fun_symbol_declContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#par_fun_symbol_decl`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitPar_fun_symbol_decl(ctx: SLComp18Parser.Par_fun_symbol_declContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#theory_attribute`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitTheory_attribute(ctx: SLComp18Parser.Theory_attributeContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#theory_decl`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitTheory_decl(ctx: SLComp18Parser.Theory_declContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#logic_attribue`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitLogic_attribue(ctx: SLComp18Parser.Logic_attribueContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#logic`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitLogic(ctx: SLComp18Parser.LogicContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#sort_dec`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitSort_dec(ctx: SLComp18Parser.Sort_decContext): SidBuilder = {
    // TODO: Is it a good idea to reuse SortDecl here (i.e., for sorts declared in datatype declarations)?
    visitChildren(ctx) match {
      case Args(symbol :: arity :: Nil) =>
        SortDecl(symbol.asInstanceOf[Symbol], arity.asInstanceOf[Numeral])
      case cs =>
        throw new Exception(s"Can't build sort dec from $cs")
    }
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#selector_dec`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitSelector_dec(ctx: SLComp18Parser.Selector_decContext): SidBuilder = {
    visitChildren(ctx) match {
      case Args(sel :: sort :: Nil) =>
        Selector(sel.asInstanceOf[Symbol], sort.asInstanceOf[Sort])
      case other => throw new Exception("Can't build selector from $other")
    }
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#constructor_dec`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitConstructor_dec(ctx: SLComp18Parser.Constructor_decContext): SidBuilder = {
    visitChildren(ctx) match {
      case Args(name :: selectors) => ConstructorDecl(name.asInstanceOf[Symbol], selectors.map(_.asInstanceOf[Selector]), None)
      case other => throw new Exception("Can't build constructor from $other")
    }
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#datatype_dec`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitDatatype_dec(ctx: SLComp18Parser.Datatype_decContext): SidBuilder = {
    // The datatype will be assembled in the visitors for declare-datatype(s)
    visitChildren(ctx)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#function_dec`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitFunction_dec(ctx: SLComp18Parser.Function_decContext): SidBuilder = {
    val args = visitChildren(ctx).toList
    // TODO: Traverse list just once
    FunDecl(args.head.asInstanceOf[Symbol], args.init.tail.map(_.asInstanceOf[SortedVar]), args.last.asInstanceOf[Sort])
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#function_def`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitFunction_def(ctx: SLComp18Parser.Function_defContext): SidBuilder = {
    val args = visitChildren(ctx).toList

    var name: Option[Symbol] = None
    var params: List[SortedVar] = Nil
    var resultType: Option[Sort] = None
    var term: Option[SidBuilder] = None
    // TODO: Just compute slices
    for (arg <- args)
      arg match {
        case s:Symbol => name = Some(s)
        case v:SortedVar => params = params :+ v
        case s:Sort => resultType = Some(s)
        case t =>
          term match {
            case None => term = Some(t)
            case Some(other) => throw new Exception(s"Can't define function as both $t and $other")
          }
      }

    if (name.isEmpty || resultType.isEmpty || term.isEmpty)
      throw new Exception(s"Incomplete function definition $args")

    FunDef(FunDecl(name.get, params, resultType.get), term.get)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#prop_literal`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitProp_literal(ctx: SLComp18Parser.Prop_literalContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#script`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitScript(ctx: SLComp18Parser.ScriptContext): SidBuilder = {
    val args = visitChildren(ctx).toList

    var sorts: List[SortDecl] = Nil
    var types: Option[DataTypes] = None
    var heap: Option[HeapDecl] = None
    var consts: List[ConstDecl] = Nil
    var funs: List[FunDef] = Nil
    var asserts: List[Assert] = Nil
    var meta: List[Meta] = Nil
    var tasks: List[Task] = Nil

    for (arg <- args)
      arg match {
        case s: SortDecl =>
          sorts = sorts :+ s
        case t: DataTypes =>
          if (types.isDefined) throw new Exception("Data types declared twice")
          types = Some(t)
        case h:HeapDecl =>
          if (heap.isDefined) throw new Exception("Heap declared twice")
          heap = Some(h)
        case c:ConstDecl => consts = consts :+ c
        case f:FunDef =>
          funs = funs :+ f
        case f:FunDefs =>
          funs = funs ++ f.defs
        case a:Assert => asserts = asserts :+ a
        case m:Meta => meta = meta :+ m
        case t:Task => tasks = tasks :+ t
        case other =>
          println(s"WARNING: Ignoring ${other}")
      }

    if (heap.isEmpty)
      throw new Exception("Missing heap declaration")
    if (funs.isEmpty)
      throw new Exception("Missing function definitions")
    if (asserts.isEmpty)
      throw new Exception("No assert => Nothing to check")

    Script(sorts, types, heap.get, consts, funs, asserts, meta, tasks)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_assert`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_assert(ctx: SLComp18Parser.Cmd_assertContext): SidBuilder = {
    CmdTypeWrapper(CmdAssert)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_checkSat`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_checkSat(ctx: SLComp18Parser.Cmd_checkSatContext): SidBuilder = {
    CmdTypeWrapper(CmdCheckSat)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_checkUnsat`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_checkUnsat(ctx: SLComp18Parser.Cmd_checkUnsatContext): SidBuilder = {
    CmdTypeWrapper(CmdCheckUnsat)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_checkSatAssuming`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_checkSatAssuming(ctx: SLComp18Parser.Cmd_checkSatAssumingContext): SidBuilder = {
    CmdTypeWrapper(CmdCheckSatAssuming)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_declareConst`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_declareConst(ctx: SLComp18Parser.Cmd_declareConstContext): SidBuilder = {
    CmdTypeWrapper(CmdDeclareConst)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_declareDatatype`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_declareDatatype(ctx: SLComp18Parser.Cmd_declareDatatypeContext): SidBuilder = {
    CmdTypeWrapper(CmdDeclareDatatype)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_declareDatatypes`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_declareDatatypes(ctx: SLComp18Parser.Cmd_declareDatatypesContext): SidBuilder = {
    CmdTypeWrapper(CmdDeclareDatatypes)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_declareFun`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_declareFun(ctx: SLComp18Parser.Cmd_declareFunContext): SidBuilder = {
    CmdTypeWrapper(CmdDeclareFun)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_declareHeap`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_declareHeap(ctx: SLComp18Parser.Cmd_declareHeapContext): SidBuilder = { CmdTypeWrapper(CmdDeclareHeap) }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_declareSort`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_declareSort(ctx: SLComp18Parser.Cmd_declareSortContext): SidBuilder = {
    CmdTypeWrapper(CmdDeclareSort)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_defineFun`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_defineFun(ctx: SLComp18Parser.Cmd_defineFunContext): SidBuilder = {
    CmdTypeWrapper(CmdDefineFun)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_defineFunRec`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_defineFunRec(ctx: SLComp18Parser.Cmd_defineFunRecContext): SidBuilder = {
    CmdTypeWrapper(CmdDefineFunRec)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_defineFunsRec`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_defineFunsRec(ctx: SLComp18Parser.Cmd_defineFunsRecContext): SidBuilder = {
    CmdTypeWrapper(CmdDefineFunsRec)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_defineSort`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_defineSort(ctx: SLComp18Parser.Cmd_defineSortContext): SidBuilder = {
    CmdTypeWrapper(CmdDefineSort)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_echo`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_echo(ctx: SLComp18Parser.Cmd_echoContext): SidBuilder = {
    CmdTypeWrapper(CmdEcho)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_exit`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_exit(ctx: SLComp18Parser.Cmd_exitContext): SidBuilder = {
    CmdTypeWrapper(CmdExit)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_getAssertions`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_getAssertions(ctx: SLComp18Parser.Cmd_getAssertionsContext): SidBuilder = {
    CmdTypeWrapper(CmdGetAssertions)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_getAssignment`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_getAssignment(ctx: SLComp18Parser.Cmd_getAssignmentContext): SidBuilder = {
    CmdTypeWrapper(CmdGetAssignment)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_getInfo`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_getInfo(ctx: SLComp18Parser.Cmd_getInfoContext): SidBuilder = {
    CmdTypeWrapper(CmdGetInfo)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_getModel`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_getModel(ctx: SLComp18Parser.Cmd_getModelContext): SidBuilder = {
    CmdTypeWrapper(CmdGetModel)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_getOption`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_getOption(ctx: SLComp18Parser.Cmd_getOptionContext): SidBuilder = {
    CmdTypeWrapper(CmdGetOption)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_getProof`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_getProof(ctx: SLComp18Parser.Cmd_getProofContext): SidBuilder = {
    CmdTypeWrapper(CmdGetProof)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_getUnsatAssumptions`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_getUnsatAssumptions(ctx: SLComp18Parser.Cmd_getUnsatAssumptionsContext): SidBuilder = {
    CmdTypeWrapper(CmdGetUnsatAssumptions)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_getUnsatCore`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_getUnsatCore(ctx: SLComp18Parser.Cmd_getUnsatCoreContext): SidBuilder = {
    CmdTypeWrapper(CmdGetUnsatCore)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_getValue`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_getValue(ctx: SLComp18Parser.Cmd_getValueContext): SidBuilder = {
    CmdTypeWrapper(CmdGetUnsatCore)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_pop`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_pop(ctx: SLComp18Parser.Cmd_popContext): SidBuilder = {
    CmdTypeWrapper(CmdPop)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_push`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_push(ctx: SLComp18Parser.Cmd_pushContext): SidBuilder = {
    CmdTypeWrapper(CmdPush)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_reset`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_reset(ctx: SLComp18Parser.Cmd_resetContext): SidBuilder = {
    CmdTypeWrapper(CmdReset)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_resetAssertions`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_resetAssertions(ctx: SLComp18Parser.Cmd_resetAssertionsContext): SidBuilder = {
    CmdTypeWrapper(CmdResetAssertions)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_setInfo`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_setInfo(ctx: SLComp18Parser.Cmd_setInfoContext): SidBuilder = {
    CmdTypeWrapper(CmdSetInfo)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_setLogic`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_setLogic(ctx: SLComp18Parser.Cmd_setLogicContext): SidBuilder = {
    CmdTypeWrapper(CmdSetLogic)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#cmd_setOption`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCmd_setOption(ctx: SLComp18Parser.Cmd_setOptionContext): SidBuilder = {
    CmdTypeWrapper(CmdSetOption)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#heap_dec`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitHeap_dec(ctx: SLComp18Parser.Heap_decContext): SidBuilder = {
    visitChildren(ctx)
  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#command`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCommand(ctx: SLComp18Parser.CommandContext): SidBuilder = {
    val args = visitChildren(ctx)
//    println(s"Got the following child args: $args")
    args match {
      case Args(commandType :: commandArgs) =>
        commandType match {
          case commandType: CmdTypeWrapper => commandType.ct.assembleCmd(commandArgs)
          case _ =>
            throw new Exception(s"Command type missing; should be at head of args $args")
        }
      case CmdTypeWrapper(ct) => ct.assembleCmd(Nil)
      case _ => throw new Exception(s"Can't build command from $args")
    }

  }

  /**
    * Visit a parse tree produced by `SLComp18Parser#b_value`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitB_value(ctx: SLComp18Parser.B_valueContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#option`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitOption(ctx: SLComp18Parser.OptionContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#info_flag`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitInfo_flag(ctx: SLComp18Parser.Info_flagContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#error_behaviour`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitError_behaviour(ctx: SLComp18Parser.Error_behaviourContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#reason_unknown`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitReason_unknown(ctx: SLComp18Parser.Reason_unknownContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#model_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitModel_response(ctx: SLComp18Parser.Model_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#info_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitInfo_response(ctx: SLComp18Parser.Info_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#valuation_pair`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitValuation_pair(ctx: SLComp18Parser.Valuation_pairContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#t_valuation_pair`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitT_valuation_pair(ctx: SLComp18Parser.T_valuation_pairContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#check_sat_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitCheck_sat_response(ctx: SLComp18Parser.Check_sat_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#echo_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitEcho_response(ctx: SLComp18Parser.Echo_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#get_assertions_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitGet_assertions_response(ctx: SLComp18Parser.Get_assertions_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#get_assignment_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitGet_assignment_response(ctx: SLComp18Parser.Get_assignment_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#get_info_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitGet_info_response(ctx: SLComp18Parser.Get_info_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#get_model_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitGet_model_response(ctx: SLComp18Parser.Get_model_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#get_option_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitGet_option_response(ctx: SLComp18Parser.Get_option_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#get_proof_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitGet_proof_response(ctx: SLComp18Parser.Get_proof_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#get_unsat_assump_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitGet_unsat_assump_response(ctx: SLComp18Parser.Get_unsat_assump_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#get_unsat_core_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitGet_unsat_core_response(ctx: SLComp18Parser.Get_unsat_core_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#get_value_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitGet_value_response(ctx: SLComp18Parser.Get_value_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#specific_success_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitSpecific_success_response(ctx: SLComp18Parser.Specific_success_responseContext): SidBuilder = fail(ctx)

  /**
    * Visit a parse tree produced by `SLComp18Parser#general_response`
    *
    * @param ctx the parse tree
    * @return the visitor result
    */
  override def visitGeneral_response(ctx: SLComp18Parser.General_responseContext): SidBuilder = fail(ctx)
}
