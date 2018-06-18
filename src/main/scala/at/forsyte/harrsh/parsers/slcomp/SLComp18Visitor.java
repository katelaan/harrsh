package at.forsyte.harrsh.parsers.slcomp;

// Generated from SLComp18.g4 by ANTLR 4.7.1
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link SLComp18Parser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface SLComp18Visitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#start}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStart(SLComp18Parser.StartContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitResponse(SLComp18Parser.ResponseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#generalReservedWord}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGeneralReservedWord(SLComp18Parser.GeneralReservedWordContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#simpleSymbol}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSimpleSymbol(SLComp18Parser.SimpleSymbolContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#quotedSymbol}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitQuotedSymbol(SLComp18Parser.QuotedSymbolContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#predefSymbol}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPredefSymbol(SLComp18Parser.PredefSymbolContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#predefKeyword}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPredefKeyword(SLComp18Parser.PredefKeywordContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#symbol}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSymbol(SLComp18Parser.SymbolContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#numeral}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNumeral(SLComp18Parser.NumeralContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#decimal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecimal(SLComp18Parser.DecimalContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#hexadecimal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitHexadecimal(SLComp18Parser.HexadecimalContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#binary}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBinary(SLComp18Parser.BinaryContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#string}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitString(SLComp18Parser.StringContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#keyword}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitKeyword(SLComp18Parser.KeywordContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#spec_constant}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSpec_constant(SLComp18Parser.Spec_constantContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#s_expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitS_expr(SLComp18Parser.S_exprContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#index}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIndex(SLComp18Parser.IndexContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#identifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdentifier(SLComp18Parser.IdentifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#attribute_value}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAttribute_value(SLComp18Parser.Attribute_valueContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#attribute}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAttribute(SLComp18Parser.AttributeContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#sort}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSort(SLComp18Parser.SortContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#qual_identifer}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitQual_identifer(SLComp18Parser.Qual_identiferContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#var_binding}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVar_binding(SLComp18Parser.Var_bindingContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#sorted_var}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSorted_var(SLComp18Parser.Sorted_varContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#pattern}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPattern(SLComp18Parser.PatternContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#match_case}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMatch_case(SLComp18Parser.Match_caseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#term}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTerm(SLComp18Parser.TermContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#sort_symbol_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSort_symbol_decl(SLComp18Parser.Sort_symbol_declContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#meta_spec_constant}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMeta_spec_constant(SLComp18Parser.Meta_spec_constantContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#fun_symbol_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFun_symbol_decl(SLComp18Parser.Fun_symbol_declContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#par_fun_symbol_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPar_fun_symbol_decl(SLComp18Parser.Par_fun_symbol_declContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#theory_attribute}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTheory_attribute(SLComp18Parser.Theory_attributeContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#theory_decl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTheory_decl(SLComp18Parser.Theory_declContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#logic_attribue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLogic_attribue(SLComp18Parser.Logic_attribueContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#logic}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLogic(SLComp18Parser.LogicContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#sort_dec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSort_dec(SLComp18Parser.Sort_decContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#selector_dec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSelector_dec(SLComp18Parser.Selector_decContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#constructor_dec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstructor_dec(SLComp18Parser.Constructor_decContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#datatype_dec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDatatype_dec(SLComp18Parser.Datatype_decContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#function_dec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunction_dec(SLComp18Parser.Function_decContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#function_def}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunction_def(SLComp18Parser.Function_defContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#prop_literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProp_literal(SLComp18Parser.Prop_literalContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#script}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitScript(SLComp18Parser.ScriptContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_assert}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_assert(SLComp18Parser.Cmd_assertContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_checkSat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_checkSat(SLComp18Parser.Cmd_checkSatContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_checkUnsat}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_checkUnsat(SLComp18Parser.Cmd_checkUnsatContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_checkSatAssuming}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_checkSatAssuming(SLComp18Parser.Cmd_checkSatAssumingContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_declareConst}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_declareConst(SLComp18Parser.Cmd_declareConstContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_declareDatatype}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_declareDatatype(SLComp18Parser.Cmd_declareDatatypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_declareDatatypes}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_declareDatatypes(SLComp18Parser.Cmd_declareDatatypesContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_declareFun}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_declareFun(SLComp18Parser.Cmd_declareFunContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_declareHeap}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_declareHeap(SLComp18Parser.Cmd_declareHeapContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_declareSort}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_declareSort(SLComp18Parser.Cmd_declareSortContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_defineFun}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_defineFun(SLComp18Parser.Cmd_defineFunContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_defineFunRec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_defineFunRec(SLComp18Parser.Cmd_defineFunRecContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_defineFunsRec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_defineFunsRec(SLComp18Parser.Cmd_defineFunsRecContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_defineSort}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_defineSort(SLComp18Parser.Cmd_defineSortContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_echo}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_echo(SLComp18Parser.Cmd_echoContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_exit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_exit(SLComp18Parser.Cmd_exitContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_getAssertions}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_getAssertions(SLComp18Parser.Cmd_getAssertionsContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_getAssignment}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_getAssignment(SLComp18Parser.Cmd_getAssignmentContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_getInfo}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_getInfo(SLComp18Parser.Cmd_getInfoContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_getModel}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_getModel(SLComp18Parser.Cmd_getModelContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_getOption}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_getOption(SLComp18Parser.Cmd_getOptionContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_getProof}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_getProof(SLComp18Parser.Cmd_getProofContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_getUnsatAssumptions}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_getUnsatAssumptions(SLComp18Parser.Cmd_getUnsatAssumptionsContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_getUnsatCore}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_getUnsatCore(SLComp18Parser.Cmd_getUnsatCoreContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_getValue}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_getValue(SLComp18Parser.Cmd_getValueContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_pop}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_pop(SLComp18Parser.Cmd_popContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_push}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_push(SLComp18Parser.Cmd_pushContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_reset}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_reset(SLComp18Parser.Cmd_resetContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_resetAssertions}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_resetAssertions(SLComp18Parser.Cmd_resetAssertionsContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_setInfo}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_setInfo(SLComp18Parser.Cmd_setInfoContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_setLogic}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_setLogic(SLComp18Parser.Cmd_setLogicContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#cmd_setOption}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmd_setOption(SLComp18Parser.Cmd_setOptionContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#heap_dec}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitHeap_dec(SLComp18Parser.Heap_decContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#command}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCommand(SLComp18Parser.CommandContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#b_value}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitB_value(SLComp18Parser.B_valueContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#option}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOption(SLComp18Parser.OptionContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#info_flag}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInfo_flag(SLComp18Parser.Info_flagContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#error_behaviour}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitError_behaviour(SLComp18Parser.Error_behaviourContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#reason_unknown}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReason_unknown(SLComp18Parser.Reason_unknownContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#model_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitModel_response(SLComp18Parser.Model_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#info_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInfo_response(SLComp18Parser.Info_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#valuation_pair}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitValuation_pair(SLComp18Parser.Valuation_pairContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#t_valuation_pair}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitT_valuation_pair(SLComp18Parser.T_valuation_pairContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#check_sat_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCheck_sat_response(SLComp18Parser.Check_sat_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#echo_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEcho_response(SLComp18Parser.Echo_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#get_assertions_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGet_assertions_response(SLComp18Parser.Get_assertions_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#get_assignment_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGet_assignment_response(SLComp18Parser.Get_assignment_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#get_info_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGet_info_response(SLComp18Parser.Get_info_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#get_model_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGet_model_response(SLComp18Parser.Get_model_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#get_option_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGet_option_response(SLComp18Parser.Get_option_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#get_proof_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGet_proof_response(SLComp18Parser.Get_proof_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#get_unsat_assump_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGet_unsat_assump_response(SLComp18Parser.Get_unsat_assump_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#get_unsat_core_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGet_unsat_core_response(SLComp18Parser.Get_unsat_core_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#get_value_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGet_value_response(SLComp18Parser.Get_value_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#specific_success_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSpecific_success_response(SLComp18Parser.Specific_success_responseContext ctx);
	/**
	 * Visit a parse tree produced by {@link SLComp18Parser#general_response}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGeneral_response(SLComp18Parser.General_responseContext ctx);
}