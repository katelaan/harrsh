package at.forsyte.harrsh.parsers.slcomp;

// Generated from SLComp18.g4 by ANTLR 4.7.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link SLComp18Parser}.
 */
public interface SLComp18Listener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#start}.
	 * @param ctx the parse tree
	 */
	void enterStart(SLComp18Parser.StartContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#start}.
	 * @param ctx the parse tree
	 */
	void exitStart(SLComp18Parser.StartContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#response}.
	 * @param ctx the parse tree
	 */
	void enterResponse(SLComp18Parser.ResponseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#response}.
	 * @param ctx the parse tree
	 */
	void exitResponse(SLComp18Parser.ResponseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#generalReservedWord}.
	 * @param ctx the parse tree
	 */
	void enterGeneralReservedWord(SLComp18Parser.GeneralReservedWordContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#generalReservedWord}.
	 * @param ctx the parse tree
	 */
	void exitGeneralReservedWord(SLComp18Parser.GeneralReservedWordContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#simpleSymbol}.
	 * @param ctx the parse tree
	 */
	void enterSimpleSymbol(SLComp18Parser.SimpleSymbolContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#simpleSymbol}.
	 * @param ctx the parse tree
	 */
	void exitSimpleSymbol(SLComp18Parser.SimpleSymbolContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#quotedSymbol}.
	 * @param ctx the parse tree
	 */
	void enterQuotedSymbol(SLComp18Parser.QuotedSymbolContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#quotedSymbol}.
	 * @param ctx the parse tree
	 */
	void exitQuotedSymbol(SLComp18Parser.QuotedSymbolContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#predefSymbol}.
	 * @param ctx the parse tree
	 */
	void enterPredefSymbol(SLComp18Parser.PredefSymbolContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#predefSymbol}.
	 * @param ctx the parse tree
	 */
	void exitPredefSymbol(SLComp18Parser.PredefSymbolContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#predefKeyword}.
	 * @param ctx the parse tree
	 */
	void enterPredefKeyword(SLComp18Parser.PredefKeywordContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#predefKeyword}.
	 * @param ctx the parse tree
	 */
	void exitPredefKeyword(SLComp18Parser.PredefKeywordContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#symbol}.
	 * @param ctx the parse tree
	 */
	void enterSymbol(SLComp18Parser.SymbolContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#symbol}.
	 * @param ctx the parse tree
	 */
	void exitSymbol(SLComp18Parser.SymbolContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#numeral}.
	 * @param ctx the parse tree
	 */
	void enterNumeral(SLComp18Parser.NumeralContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#numeral}.
	 * @param ctx the parse tree
	 */
	void exitNumeral(SLComp18Parser.NumeralContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#decimal}.
	 * @param ctx the parse tree
	 */
	void enterDecimal(SLComp18Parser.DecimalContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#decimal}.
	 * @param ctx the parse tree
	 */
	void exitDecimal(SLComp18Parser.DecimalContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#hexadecimal}.
	 * @param ctx the parse tree
	 */
	void enterHexadecimal(SLComp18Parser.HexadecimalContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#hexadecimal}.
	 * @param ctx the parse tree
	 */
	void exitHexadecimal(SLComp18Parser.HexadecimalContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#binary}.
	 * @param ctx the parse tree
	 */
	void enterBinary(SLComp18Parser.BinaryContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#binary}.
	 * @param ctx the parse tree
	 */
	void exitBinary(SLComp18Parser.BinaryContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#string}.
	 * @param ctx the parse tree
	 */
	void enterString(SLComp18Parser.StringContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#string}.
	 * @param ctx the parse tree
	 */
	void exitString(SLComp18Parser.StringContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#keyword}.
	 * @param ctx the parse tree
	 */
	void enterKeyword(SLComp18Parser.KeywordContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#keyword}.
	 * @param ctx the parse tree
	 */
	void exitKeyword(SLComp18Parser.KeywordContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#spec_constant}.
	 * @param ctx the parse tree
	 */
	void enterSpec_constant(SLComp18Parser.Spec_constantContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#spec_constant}.
	 * @param ctx the parse tree
	 */
	void exitSpec_constant(SLComp18Parser.Spec_constantContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#s_expr}.
	 * @param ctx the parse tree
	 */
	void enterS_expr(SLComp18Parser.S_exprContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#s_expr}.
	 * @param ctx the parse tree
	 */
	void exitS_expr(SLComp18Parser.S_exprContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#index}.
	 * @param ctx the parse tree
	 */
	void enterIndex(SLComp18Parser.IndexContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#index}.
	 * @param ctx the parse tree
	 */
	void exitIndex(SLComp18Parser.IndexContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#identifier}.
	 * @param ctx the parse tree
	 */
	void enterIdentifier(SLComp18Parser.IdentifierContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#identifier}.
	 * @param ctx the parse tree
	 */
	void exitIdentifier(SLComp18Parser.IdentifierContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#attribute_value}.
	 * @param ctx the parse tree
	 */
	void enterAttribute_value(SLComp18Parser.Attribute_valueContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#attribute_value}.
	 * @param ctx the parse tree
	 */
	void exitAttribute_value(SLComp18Parser.Attribute_valueContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#attribute}.
	 * @param ctx the parse tree
	 */
	void enterAttribute(SLComp18Parser.AttributeContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#attribute}.
	 * @param ctx the parse tree
	 */
	void exitAttribute(SLComp18Parser.AttributeContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#sort}.
	 * @param ctx the parse tree
	 */
	void enterSort(SLComp18Parser.SortContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#sort}.
	 * @param ctx the parse tree
	 */
	void exitSort(SLComp18Parser.SortContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#qual_identifer}.
	 * @param ctx the parse tree
	 */
	void enterQual_identifer(SLComp18Parser.Qual_identiferContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#qual_identifer}.
	 * @param ctx the parse tree
	 */
	void exitQual_identifer(SLComp18Parser.Qual_identiferContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#var_binding}.
	 * @param ctx the parse tree
	 */
	void enterVar_binding(SLComp18Parser.Var_bindingContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#var_binding}.
	 * @param ctx the parse tree
	 */
	void exitVar_binding(SLComp18Parser.Var_bindingContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#sorted_var}.
	 * @param ctx the parse tree
	 */
	void enterSorted_var(SLComp18Parser.Sorted_varContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#sorted_var}.
	 * @param ctx the parse tree
	 */
	void exitSorted_var(SLComp18Parser.Sorted_varContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#pattern}.
	 * @param ctx the parse tree
	 */
	void enterPattern(SLComp18Parser.PatternContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#pattern}.
	 * @param ctx the parse tree
	 */
	void exitPattern(SLComp18Parser.PatternContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#match_case}.
	 * @param ctx the parse tree
	 */
	void enterMatch_case(SLComp18Parser.Match_caseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#match_case}.
	 * @param ctx the parse tree
	 */
	void exitMatch_case(SLComp18Parser.Match_caseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#term}.
	 * @param ctx the parse tree
	 */
	void enterTerm(SLComp18Parser.TermContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#term}.
	 * @param ctx the parse tree
	 */
	void exitTerm(SLComp18Parser.TermContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#sort_symbol_decl}.
	 * @param ctx the parse tree
	 */
	void enterSort_symbol_decl(SLComp18Parser.Sort_symbol_declContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#sort_symbol_decl}.
	 * @param ctx the parse tree
	 */
	void exitSort_symbol_decl(SLComp18Parser.Sort_symbol_declContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#meta_spec_constant}.
	 * @param ctx the parse tree
	 */
	void enterMeta_spec_constant(SLComp18Parser.Meta_spec_constantContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#meta_spec_constant}.
	 * @param ctx the parse tree
	 */
	void exitMeta_spec_constant(SLComp18Parser.Meta_spec_constantContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#fun_symbol_decl}.
	 * @param ctx the parse tree
	 */
	void enterFun_symbol_decl(SLComp18Parser.Fun_symbol_declContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#fun_symbol_decl}.
	 * @param ctx the parse tree
	 */
	void exitFun_symbol_decl(SLComp18Parser.Fun_symbol_declContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#par_fun_symbol_decl}.
	 * @param ctx the parse tree
	 */
	void enterPar_fun_symbol_decl(SLComp18Parser.Par_fun_symbol_declContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#par_fun_symbol_decl}.
	 * @param ctx the parse tree
	 */
	void exitPar_fun_symbol_decl(SLComp18Parser.Par_fun_symbol_declContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#theory_attribute}.
	 * @param ctx the parse tree
	 */
	void enterTheory_attribute(SLComp18Parser.Theory_attributeContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#theory_attribute}.
	 * @param ctx the parse tree
	 */
	void exitTheory_attribute(SLComp18Parser.Theory_attributeContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#theory_decl}.
	 * @param ctx the parse tree
	 */
	void enterTheory_decl(SLComp18Parser.Theory_declContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#theory_decl}.
	 * @param ctx the parse tree
	 */
	void exitTheory_decl(SLComp18Parser.Theory_declContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#logic_attribue}.
	 * @param ctx the parse tree
	 */
	void enterLogic_attribue(SLComp18Parser.Logic_attribueContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#logic_attribue}.
	 * @param ctx the parse tree
	 */
	void exitLogic_attribue(SLComp18Parser.Logic_attribueContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#logic}.
	 * @param ctx the parse tree
	 */
	void enterLogic(SLComp18Parser.LogicContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#logic}.
	 * @param ctx the parse tree
	 */
	void exitLogic(SLComp18Parser.LogicContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#sort_dec}.
	 * @param ctx the parse tree
	 */
	void enterSort_dec(SLComp18Parser.Sort_decContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#sort_dec}.
	 * @param ctx the parse tree
	 */
	void exitSort_dec(SLComp18Parser.Sort_decContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#selector_dec}.
	 * @param ctx the parse tree
	 */
	void enterSelector_dec(SLComp18Parser.Selector_decContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#selector_dec}.
	 * @param ctx the parse tree
	 */
	void exitSelector_dec(SLComp18Parser.Selector_decContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#constructor_dec}.
	 * @param ctx the parse tree
	 */
	void enterConstructor_dec(SLComp18Parser.Constructor_decContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#constructor_dec}.
	 * @param ctx the parse tree
	 */
	void exitConstructor_dec(SLComp18Parser.Constructor_decContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#datatype_dec}.
	 * @param ctx the parse tree
	 */
	void enterDatatype_dec(SLComp18Parser.Datatype_decContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#datatype_dec}.
	 * @param ctx the parse tree
	 */
	void exitDatatype_dec(SLComp18Parser.Datatype_decContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#function_dec}.
	 * @param ctx the parse tree
	 */
	void enterFunction_dec(SLComp18Parser.Function_decContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#function_dec}.
	 * @param ctx the parse tree
	 */
	void exitFunction_dec(SLComp18Parser.Function_decContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#function_def}.
	 * @param ctx the parse tree
	 */
	void enterFunction_def(SLComp18Parser.Function_defContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#function_def}.
	 * @param ctx the parse tree
	 */
	void exitFunction_def(SLComp18Parser.Function_defContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#prop_literal}.
	 * @param ctx the parse tree
	 */
	void enterProp_literal(SLComp18Parser.Prop_literalContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#prop_literal}.
	 * @param ctx the parse tree
	 */
	void exitProp_literal(SLComp18Parser.Prop_literalContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#script}.
	 * @param ctx the parse tree
	 */
	void enterScript(SLComp18Parser.ScriptContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#script}.
	 * @param ctx the parse tree
	 */
	void exitScript(SLComp18Parser.ScriptContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_assert}.
	 * @param ctx the parse tree
	 */
	void enterCmd_assert(SLComp18Parser.Cmd_assertContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_assert}.
	 * @param ctx the parse tree
	 */
	void exitCmd_assert(SLComp18Parser.Cmd_assertContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_checkSat}.
	 * @param ctx the parse tree
	 */
	void enterCmd_checkSat(SLComp18Parser.Cmd_checkSatContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_checkSat}.
	 * @param ctx the parse tree
	 */
	void exitCmd_checkSat(SLComp18Parser.Cmd_checkSatContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_checkUnsat}.
	 * @param ctx the parse tree
	 */
	void enterCmd_checkUnsat(SLComp18Parser.Cmd_checkUnsatContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_checkUnsat}.
	 * @param ctx the parse tree
	 */
	void exitCmd_checkUnsat(SLComp18Parser.Cmd_checkUnsatContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_checkSatAssuming}.
	 * @param ctx the parse tree
	 */
	void enterCmd_checkSatAssuming(SLComp18Parser.Cmd_checkSatAssumingContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_checkSatAssuming}.
	 * @param ctx the parse tree
	 */
	void exitCmd_checkSatAssuming(SLComp18Parser.Cmd_checkSatAssumingContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_declareConst}.
	 * @param ctx the parse tree
	 */
	void enterCmd_declareConst(SLComp18Parser.Cmd_declareConstContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_declareConst}.
	 * @param ctx the parse tree
	 */
	void exitCmd_declareConst(SLComp18Parser.Cmd_declareConstContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_declareDatatype}.
	 * @param ctx the parse tree
	 */
	void enterCmd_declareDatatype(SLComp18Parser.Cmd_declareDatatypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_declareDatatype}.
	 * @param ctx the parse tree
	 */
	void exitCmd_declareDatatype(SLComp18Parser.Cmd_declareDatatypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_declareDatatypes}.
	 * @param ctx the parse tree
	 */
	void enterCmd_declareDatatypes(SLComp18Parser.Cmd_declareDatatypesContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_declareDatatypes}.
	 * @param ctx the parse tree
	 */
	void exitCmd_declareDatatypes(SLComp18Parser.Cmd_declareDatatypesContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_declareFun}.
	 * @param ctx the parse tree
	 */
	void enterCmd_declareFun(SLComp18Parser.Cmd_declareFunContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_declareFun}.
	 * @param ctx the parse tree
	 */
	void exitCmd_declareFun(SLComp18Parser.Cmd_declareFunContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_declareHeap}.
	 * @param ctx the parse tree
	 */
	void enterCmd_declareHeap(SLComp18Parser.Cmd_declareHeapContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_declareHeap}.
	 * @param ctx the parse tree
	 */
	void exitCmd_declareHeap(SLComp18Parser.Cmd_declareHeapContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_declareSort}.
	 * @param ctx the parse tree
	 */
	void enterCmd_declareSort(SLComp18Parser.Cmd_declareSortContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_declareSort}.
	 * @param ctx the parse tree
	 */
	void exitCmd_declareSort(SLComp18Parser.Cmd_declareSortContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_defineFun}.
	 * @param ctx the parse tree
	 */
	void enterCmd_defineFun(SLComp18Parser.Cmd_defineFunContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_defineFun}.
	 * @param ctx the parse tree
	 */
	void exitCmd_defineFun(SLComp18Parser.Cmd_defineFunContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_defineFunRec}.
	 * @param ctx the parse tree
	 */
	void enterCmd_defineFunRec(SLComp18Parser.Cmd_defineFunRecContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_defineFunRec}.
	 * @param ctx the parse tree
	 */
	void exitCmd_defineFunRec(SLComp18Parser.Cmd_defineFunRecContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_defineFunsRec}.
	 * @param ctx the parse tree
	 */
	void enterCmd_defineFunsRec(SLComp18Parser.Cmd_defineFunsRecContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_defineFunsRec}.
	 * @param ctx the parse tree
	 */
	void exitCmd_defineFunsRec(SLComp18Parser.Cmd_defineFunsRecContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_defineSort}.
	 * @param ctx the parse tree
	 */
	void enterCmd_defineSort(SLComp18Parser.Cmd_defineSortContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_defineSort}.
	 * @param ctx the parse tree
	 */
	void exitCmd_defineSort(SLComp18Parser.Cmd_defineSortContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_echo}.
	 * @param ctx the parse tree
	 */
	void enterCmd_echo(SLComp18Parser.Cmd_echoContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_echo}.
	 * @param ctx the parse tree
	 */
	void exitCmd_echo(SLComp18Parser.Cmd_echoContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_exit}.
	 * @param ctx the parse tree
	 */
	void enterCmd_exit(SLComp18Parser.Cmd_exitContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_exit}.
	 * @param ctx the parse tree
	 */
	void exitCmd_exit(SLComp18Parser.Cmd_exitContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_getAssertions}.
	 * @param ctx the parse tree
	 */
	void enterCmd_getAssertions(SLComp18Parser.Cmd_getAssertionsContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_getAssertions}.
	 * @param ctx the parse tree
	 */
	void exitCmd_getAssertions(SLComp18Parser.Cmd_getAssertionsContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_getAssignment}.
	 * @param ctx the parse tree
	 */
	void enterCmd_getAssignment(SLComp18Parser.Cmd_getAssignmentContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_getAssignment}.
	 * @param ctx the parse tree
	 */
	void exitCmd_getAssignment(SLComp18Parser.Cmd_getAssignmentContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_getInfo}.
	 * @param ctx the parse tree
	 */
	void enterCmd_getInfo(SLComp18Parser.Cmd_getInfoContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_getInfo}.
	 * @param ctx the parse tree
	 */
	void exitCmd_getInfo(SLComp18Parser.Cmd_getInfoContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_getModel}.
	 * @param ctx the parse tree
	 */
	void enterCmd_getModel(SLComp18Parser.Cmd_getModelContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_getModel}.
	 * @param ctx the parse tree
	 */
	void exitCmd_getModel(SLComp18Parser.Cmd_getModelContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_getOption}.
	 * @param ctx the parse tree
	 */
	void enterCmd_getOption(SLComp18Parser.Cmd_getOptionContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_getOption}.
	 * @param ctx the parse tree
	 */
	void exitCmd_getOption(SLComp18Parser.Cmd_getOptionContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_getProof}.
	 * @param ctx the parse tree
	 */
	void enterCmd_getProof(SLComp18Parser.Cmd_getProofContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_getProof}.
	 * @param ctx the parse tree
	 */
	void exitCmd_getProof(SLComp18Parser.Cmd_getProofContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_getUnsatAssumptions}.
	 * @param ctx the parse tree
	 */
	void enterCmd_getUnsatAssumptions(SLComp18Parser.Cmd_getUnsatAssumptionsContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_getUnsatAssumptions}.
	 * @param ctx the parse tree
	 */
	void exitCmd_getUnsatAssumptions(SLComp18Parser.Cmd_getUnsatAssumptionsContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_getUnsatCore}.
	 * @param ctx the parse tree
	 */
	void enterCmd_getUnsatCore(SLComp18Parser.Cmd_getUnsatCoreContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_getUnsatCore}.
	 * @param ctx the parse tree
	 */
	void exitCmd_getUnsatCore(SLComp18Parser.Cmd_getUnsatCoreContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_getValue}.
	 * @param ctx the parse tree
	 */
	void enterCmd_getValue(SLComp18Parser.Cmd_getValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_getValue}.
	 * @param ctx the parse tree
	 */
	void exitCmd_getValue(SLComp18Parser.Cmd_getValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_pop}.
	 * @param ctx the parse tree
	 */
	void enterCmd_pop(SLComp18Parser.Cmd_popContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_pop}.
	 * @param ctx the parse tree
	 */
	void exitCmd_pop(SLComp18Parser.Cmd_popContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_push}.
	 * @param ctx the parse tree
	 */
	void enterCmd_push(SLComp18Parser.Cmd_pushContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_push}.
	 * @param ctx the parse tree
	 */
	void exitCmd_push(SLComp18Parser.Cmd_pushContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_reset}.
	 * @param ctx the parse tree
	 */
	void enterCmd_reset(SLComp18Parser.Cmd_resetContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_reset}.
	 * @param ctx the parse tree
	 */
	void exitCmd_reset(SLComp18Parser.Cmd_resetContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_resetAssertions}.
	 * @param ctx the parse tree
	 */
	void enterCmd_resetAssertions(SLComp18Parser.Cmd_resetAssertionsContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_resetAssertions}.
	 * @param ctx the parse tree
	 */
	void exitCmd_resetAssertions(SLComp18Parser.Cmd_resetAssertionsContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_setInfo}.
	 * @param ctx the parse tree
	 */
	void enterCmd_setInfo(SLComp18Parser.Cmd_setInfoContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_setInfo}.
	 * @param ctx the parse tree
	 */
	void exitCmd_setInfo(SLComp18Parser.Cmd_setInfoContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_setLogic}.
	 * @param ctx the parse tree
	 */
	void enterCmd_setLogic(SLComp18Parser.Cmd_setLogicContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_setLogic}.
	 * @param ctx the parse tree
	 */
	void exitCmd_setLogic(SLComp18Parser.Cmd_setLogicContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#cmd_setOption}.
	 * @param ctx the parse tree
	 */
	void enterCmd_setOption(SLComp18Parser.Cmd_setOptionContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#cmd_setOption}.
	 * @param ctx the parse tree
	 */
	void exitCmd_setOption(SLComp18Parser.Cmd_setOptionContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#heap_dec}.
	 * @param ctx the parse tree
	 */
	void enterHeap_dec(SLComp18Parser.Heap_decContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#heap_dec}.
	 * @param ctx the parse tree
	 */
	void exitHeap_dec(SLComp18Parser.Heap_decContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#command}.
	 * @param ctx the parse tree
	 */
	void enterCommand(SLComp18Parser.CommandContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#command}.
	 * @param ctx the parse tree
	 */
	void exitCommand(SLComp18Parser.CommandContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#b_value}.
	 * @param ctx the parse tree
	 */
	void enterB_value(SLComp18Parser.B_valueContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#b_value}.
	 * @param ctx the parse tree
	 */
	void exitB_value(SLComp18Parser.B_valueContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#option}.
	 * @param ctx the parse tree
	 */
	void enterOption(SLComp18Parser.OptionContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#option}.
	 * @param ctx the parse tree
	 */
	void exitOption(SLComp18Parser.OptionContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#info_flag}.
	 * @param ctx the parse tree
	 */
	void enterInfo_flag(SLComp18Parser.Info_flagContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#info_flag}.
	 * @param ctx the parse tree
	 */
	void exitInfo_flag(SLComp18Parser.Info_flagContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#error_behaviour}.
	 * @param ctx the parse tree
	 */
	void enterError_behaviour(SLComp18Parser.Error_behaviourContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#error_behaviour}.
	 * @param ctx the parse tree
	 */
	void exitError_behaviour(SLComp18Parser.Error_behaviourContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#reason_unknown}.
	 * @param ctx the parse tree
	 */
	void enterReason_unknown(SLComp18Parser.Reason_unknownContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#reason_unknown}.
	 * @param ctx the parse tree
	 */
	void exitReason_unknown(SLComp18Parser.Reason_unknownContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#model_response}.
	 * @param ctx the parse tree
	 */
	void enterModel_response(SLComp18Parser.Model_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#model_response}.
	 * @param ctx the parse tree
	 */
	void exitModel_response(SLComp18Parser.Model_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#info_response}.
	 * @param ctx the parse tree
	 */
	void enterInfo_response(SLComp18Parser.Info_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#info_response}.
	 * @param ctx the parse tree
	 */
	void exitInfo_response(SLComp18Parser.Info_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#valuation_pair}.
	 * @param ctx the parse tree
	 */
	void enterValuation_pair(SLComp18Parser.Valuation_pairContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#valuation_pair}.
	 * @param ctx the parse tree
	 */
	void exitValuation_pair(SLComp18Parser.Valuation_pairContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#t_valuation_pair}.
	 * @param ctx the parse tree
	 */
	void enterT_valuation_pair(SLComp18Parser.T_valuation_pairContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#t_valuation_pair}.
	 * @param ctx the parse tree
	 */
	void exitT_valuation_pair(SLComp18Parser.T_valuation_pairContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#check_sat_response}.
	 * @param ctx the parse tree
	 */
	void enterCheck_sat_response(SLComp18Parser.Check_sat_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#check_sat_response}.
	 * @param ctx the parse tree
	 */
	void exitCheck_sat_response(SLComp18Parser.Check_sat_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#echo_response}.
	 * @param ctx the parse tree
	 */
	void enterEcho_response(SLComp18Parser.Echo_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#echo_response}.
	 * @param ctx the parse tree
	 */
	void exitEcho_response(SLComp18Parser.Echo_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#get_assertions_response}.
	 * @param ctx the parse tree
	 */
	void enterGet_assertions_response(SLComp18Parser.Get_assertions_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#get_assertions_response}.
	 * @param ctx the parse tree
	 */
	void exitGet_assertions_response(SLComp18Parser.Get_assertions_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#get_assignment_response}.
	 * @param ctx the parse tree
	 */
	void enterGet_assignment_response(SLComp18Parser.Get_assignment_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#get_assignment_response}.
	 * @param ctx the parse tree
	 */
	void exitGet_assignment_response(SLComp18Parser.Get_assignment_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#get_info_response}.
	 * @param ctx the parse tree
	 */
	void enterGet_info_response(SLComp18Parser.Get_info_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#get_info_response}.
	 * @param ctx the parse tree
	 */
	void exitGet_info_response(SLComp18Parser.Get_info_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#get_model_response}.
	 * @param ctx the parse tree
	 */
	void enterGet_model_response(SLComp18Parser.Get_model_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#get_model_response}.
	 * @param ctx the parse tree
	 */
	void exitGet_model_response(SLComp18Parser.Get_model_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#get_option_response}.
	 * @param ctx the parse tree
	 */
	void enterGet_option_response(SLComp18Parser.Get_option_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#get_option_response}.
	 * @param ctx the parse tree
	 */
	void exitGet_option_response(SLComp18Parser.Get_option_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#get_proof_response}.
	 * @param ctx the parse tree
	 */
	void enterGet_proof_response(SLComp18Parser.Get_proof_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#get_proof_response}.
	 * @param ctx the parse tree
	 */
	void exitGet_proof_response(SLComp18Parser.Get_proof_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#get_unsat_assump_response}.
	 * @param ctx the parse tree
	 */
	void enterGet_unsat_assump_response(SLComp18Parser.Get_unsat_assump_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#get_unsat_assump_response}.
	 * @param ctx the parse tree
	 */
	void exitGet_unsat_assump_response(SLComp18Parser.Get_unsat_assump_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#get_unsat_core_response}.
	 * @param ctx the parse tree
	 */
	void enterGet_unsat_core_response(SLComp18Parser.Get_unsat_core_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#get_unsat_core_response}.
	 * @param ctx the parse tree
	 */
	void exitGet_unsat_core_response(SLComp18Parser.Get_unsat_core_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#get_value_response}.
	 * @param ctx the parse tree
	 */
	void enterGet_value_response(SLComp18Parser.Get_value_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#get_value_response}.
	 * @param ctx the parse tree
	 */
	void exitGet_value_response(SLComp18Parser.Get_value_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#specific_success_response}.
	 * @param ctx the parse tree
	 */
	void enterSpecific_success_response(SLComp18Parser.Specific_success_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#specific_success_response}.
	 * @param ctx the parse tree
	 */
	void exitSpecific_success_response(SLComp18Parser.Specific_success_responseContext ctx);
	/**
	 * Enter a parse tree produced by {@link SLComp18Parser#general_response}.
	 * @param ctx the parse tree
	 */
	void enterGeneral_response(SLComp18Parser.General_responseContext ctx);
	/**
	 * Exit a parse tree produced by {@link SLComp18Parser#general_response}.
	 * @param ctx the parse tree
	 */
	void exitGeneral_response(SLComp18Parser.General_responseContext ctx);
}