package vct.parsers

import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext}
import vct.antlr4.generated.CPPParser
import vct.antlr4.generated.CPPParser._
import vct.antlr4.generated.CPPParserPatterns._
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort, Type}
import vct.col.ast.expr.{NameExpression, NameExpressionKind, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.Method.Kind
import vct.col.ast.stmt.decl.{ASTDeclaration, ASTSpecial, DeclarationStatement, Method, ProgramUnit, VariableDeclaration}
import vct.col.ast.util.ContractBuilder
import vct.col.ast.expr.StandardOperator._


object CPPtoCOL {
  def convert(tree: TranslationunitContext, fileName: String, tokens: CommonTokenStream, parser: CPPParser): ProgramUnit = {
    CPPtoCOL(fileName, tokens, parser).convertProgram(tree)
  }
}

case class CPPtoCOL(fileName: String, tokens: CommonTokenStream, parser: CPPParser)
  extends ToCOL(fileName, tokens, parser) {

  def convertProgram(tree: TranslationunitContext): ProgramUnit = tree match {
    case Translationunit0(None, _) => new ProgramUnit()
    case Translationunit0(Some(declSeq), _) =>  {
      val pu = new ProgramUnit()
      convertDeclarationseq(declSeq).foreach(pu.add)
      pu
    }
  }

  def convertDeclarationseq(tree: DeclarationseqContext): Seq[ASTDeclaration]  = tree match {
    case Declarationseq0(decl) => Seq(convertDeclaration(decl))
    case Declarationseq1(declseq, decl) => convertDeclarationseq(declseq) ++ Seq(convertDeclaration(decl))
  }

  def convertDeclaration(tree: DeclarationContext): ASTDeclaration = tree match {
    case Declaration0(block) => fail(tree, "Unsupported syntax")
    case Declaration1(func) => {
      val function = convertFunctiondefinition(func)
      function.setStatic(true)
      function
    }
    case Declaration2(template) => fail(tree, "Unsupported syntax")
    case Declaration3(explInstantiation) => fail(tree, "Unsupported syntax")
    case Declaration4(explSpecialization) => fail(tree, "Unsupported syntax")
    case Declaration5(link) => fail(tree, "Unsupported syntax")
    case Declaration6(namespace) => fail(tree, "Unsupported syntax")
    case Declaration7(empty) => fail(tree, "Unsupported syntax")
    case Declaration8(attr) => fail(tree, "Unsupported syntax")
  }

  def convertFunctiondefinition(tree: FunctiondefinitionContext): ASTDeclaration = origin(tree, tree match {
    case Functiondefinition0(maybeContract, maybeAttrSeq, maybeDeclSpecSeq, declarator, maybeVirtSpecSeq, funcBody) => {
      val name = convertDeclarator(declarator)
      val body: ASTNode = convertFunctionbody(funcBody)
      val attrseq = maybeAttrSeq match {
        case None => Seq()
        case Some(attrSeq) => ???
      }
      val declspecseq = maybeDeclSpecSeq match {
        case None => create primitive_type(PrimitiveSort.Void)
        case Some(declSpecSeq) => convertDeclspecifierseq(declSpecSeq)
      }
      val virtspecseq = maybeVirtSpecSeq match {
        case None => Seq()
        case Some(virtSpecSeq) => ???
      }
      val contract = getContract(convertValContract(maybeContract))
      //TODO contract, arguments, kind, declspecseq(which can contain the kind etc)
      create.method_kind(Kind.Plain, declspecseq, null /*TODO contract*/,
        name, Seq.empty[DeclarationStatement].toArray, body)//maybeArgs.map(convertArgs).getOrElse(Seq()).toArray, body.orNull)
    }
  })

  def expr(tree: ParserRuleContext): ASTNode = origin(tree, tree match {
    //
    case Expression0(assignmentExpr) => expr(assignmentExpr)
    case Expression1(expr, _, assignmentExpr) => fail(tree, "Unsupported syntax")

    case Assignmentexpression0(conditionalexpr) => expr(conditionalexpr)
    case Assignmentexpression1(logicalor, assignop, initclause) => {
      assignop match {
        case Assignmentoperator0("=") => {}
        case _ => fail(assignop, "Unsupported syntax")
      }
      create assignment(
        expr(logicalor),
        convertInitializerclause(initclause)
      )
    }
    case Assignmentexpression2(throwexpr) => fail(tree, "Unsupported syntax")

    case Conditionalexpression0(logicalExpr) => expr(logicalExpr)
    case Conditionalexpression1(cond, _, yes, _, no) => create expression(StandardOperator.ITE, expr(cond), expr(yes), expr(no))

    case Logicalorexpression0(andexpr) => expr(andexpr)
    case Logicalorexpression1(lft, "||", rght) => create expression(StandardOperator.Or, expr(lft), expr(rght))
    case Logicalorexpression1(lft, "or", rght) => create expression(StandardOperator.Or, expr(lft), expr(rght))

    case Logicalandexpression0(inclusiveorexpr) => expr(inclusiveorexpr)
    case Logicalandexpression1(lft, "&&", rght) => create expression(StandardOperator.And, expr(lft), expr(rght))
    case Logicalandexpression1(lft, "and", rght) => create expression(StandardOperator.And, expr(lft), expr(rght))

    case Inclusiveorexpression0(exclexpr) => expr(exclexpr)
    case Inclusiveorexpression1(lft, "|", rght) => create expression(StandardOperator.BitOr, expr(lft), expr(rght))

    case Exclusiveorexpression0(andexpr) => expr(andexpr)
    case Exclusiveorexpression1(lft, "^", rght) => create expression(StandardOperator.BitXor, expr(lft), expr(rght))

    case Andexpression0(eqexpr) => expr(eqexpr)
    case Andexpression1(lft, "&", rght) => create expression(StandardOperator.BitAnd, expr(lft), expr(rght))

    case Equalityexpression0(relationalexpr) => expr(relationalexpr)
    case Equalityexpression1(lft, "==", rght) => create expression(StandardOperator.EQ, expr(lft), expr(rght))
    case Equalityexpression1(lft, "!=", rght) => create expression(StandardOperator.NEQ, expr(lft), expr(rght))

    case Relationalexpression0(shiftexpr) => expr(shiftexpr)
    case Relationalexpression1(lft, "<", rght) => create expression(StandardOperator.LT, expr(lft), expr(rght))
    case Relationalexpression2(lft, ">", rght) => create expression(StandardOperator.GT, expr(lft), expr(rght))
    case Relationalexpression3(lft, "<=", rght) => create expression(StandardOperator.LTE, expr(lft), expr(rght))
    case Relationalexpression4(lft, ">=", rght) => create expression(StandardOperator.GTE, expr(lft), expr(rght))

    case Shiftexpression0(additiveexpr) => expr(additiveexpr)
    case Shiftexpression1(lft, shiftop, rght) => {
      create expression(
        shiftop match {
          case Shiftoperator0(">>") => StandardOperator.RightShift
          case Shiftoperator1("<<") => StandardOperator.LeftShift
        },
        expr(lft),
        expr(rght)
      )
    }

    case Additiveexpression0(mulexpr) => expr(mulexpr)
    case Additiveexpression1(lft, "+", rght) => create expression(StandardOperator.Plus, expr(lft), expr(rght))
    case Additiveexpression2(lft, "-", rght) => create expression(StandardOperator.Minus, expr(lft), expr(rght))

    case Multiplicativeexpression0(pmexpr) => expr(pmexpr)
    case Multiplicativeexpression1(lft, "*", rght) => create expression(StandardOperator.Mult, expr(lft), expr(rght))
    case Multiplicativeexpression2(lft, "/", rght) => create expression(StandardOperator.Div, expr(lft), expr(rght))
    case Multiplicativeexpression3(lft, "%", rght) => create expression(StandardOperator.Mod, expr(lft), expr(rght))

    case Pmexpression0(castexpr) => expr(castexpr)
    case Pmexpression1(lft, ".*", rght) => fail(tree, "Unsupported syntax")
    case Pmexpression2(lft, "->*", rght) => fail(tree, "Unsupported syntax")

    case Castexpression0(unaryexpr) => expr(unaryexpr)
    case Castexpression1("(", thetypeid, ")", castexpr) => fail(tree, "Unsupported syntax")

    case Unaryexpression0(postfixexpr) => expr(postfixexpr)
    //Other cases ommited

    case Postfixexpression0(primaryexpr) => expr(primaryexpr)
    //Other cases ommited

    case Primaryexpression0(literalexpr) => expr(literalexpr)
//    case Primaryexpression1(_) => create this_expression()
    case Primaryexpression3(idexpr) => create unresolved_name convertIdexpression(idexpr)
    case Primaryexpression5(valPrimary) => valExpr(valPrimary)
    case Literal0(integer) => create constant Integer.parseInt(integer)
    case Literal4(bool) => bool match {
      case Booleanliteral0("false") => create constant(false)
      case Booleanliteral1("true") => create constant(true)
    }

    case Condition0(condexpr) => expr(condexpr)
  })

  def convertFunctionbody(tree: FunctionbodyContext): BlockStatement= origin(tree, tree match {
    case Functionbody0(_, Compoundstatement0("{", None, "}")) => new BlockStatement() //TODO what was the ASTNode for a body
    case Functionbody0(_, Compoundstatement0("{", Some(stmntSeq), "}")) => {
      val block = new BlockStatement()
      convertStatementseq(stmntSeq).foreach(block.add)
      block
    }
  })

  def convertStatementseq(tree: StatementseqContext): Seq[ASTNode] = origin(tree, tree match {
    case Statementseq0(stmnt) => Seq(convertStatement(stmnt))
    case Statementseq1(stmntseq, stmnt) => convertStatementseq(stmntseq) :+ convertStatement(stmnt)
  })

  def convertStatement(tree: ParserRuleContext): ASTNode = tree match {
    case Statement0(labeledStmnt) => convertStatement(labeledStmnt)
    case Statement1(_, exprStmnt) => convertStatement(exprStmnt)
    case Statement2(_, compoundStmnt) => convertStatement(compoundStmnt)
    case Statement3(_, selectionStmnt) => convertStatement(selectionStmnt)
    case Statement4(_, iterationStmnt) => convertStatement(iterationStmnt)
    case Statement5(_, jumpStmnt) => convertStatement(jumpStmnt)
    case Statement6(declStmnt) => convertStatement(declStmnt)
    case Statement7(_, tryblock) => convertStatement(tryblock)
    case Statement8(valstmnt) => create block (convertValStat(valstmnt):_*)
    case Statement9(valstmnt) => convertValStat(valstmnt)

    case Labeledstatement0(_) => fail(tree, "Unsupported syntax")
    case Labeledstatement1(_) => fail(tree, "Unsupported syntax")
    case Labeledstatement2(_) => fail(tree, "Unsupported syntax")

    case Expressionstatement0(None, _) => new BlockStatement
    case Expressionstatement0(Some(exprstmnt), _) => expr(exprstmnt)

    case Compoundstatement0(_, None, _) => new BlockStatement
    case Compoundstatement0(_, Some(stmntSeq), _) => create.block(convertStatementseq(stmntSeq):_*)

    case Selectionstatement0("if", "(", condition, ")", ifstmnt) => {
      create ifthenelse(expr(condition), convertStatement(ifstmnt), null)
    }
    case Selectionstatement1("if", "(", condition, ")", ifstmnt, "else", elsestmnt) => {
      create ifthenelse(expr(condition), convertStatement(ifstmnt), convertStatement(elsestmnt))
    }
    case Selectionstatement2("switch", "(", condition, ")", switchstmnt) => fail(tree, "Unsupported syntax")

    case Iterationstatement0(maybeContract1, "while", "(", condition, ")", maybeContract2, stmnt) => {
      create while_loop(expr(condition), convertStatement(stmnt), getContract(convertValContract(maybeContract1), convertValContract(maybeContract2)))
    }
    case Iterationstatement1(maybeContr1, "do", stmnt, "while", "(", expr, ")", _) => fail(tree, "Unsupported syntax")
    case Iterationstatement2(maybeContr1, "for", "(", _, _, ";", _, ")", maybeContr2, stmnt) => fail(tree, "Unsupported syntax")
    case Iterationstatement3(maybeContr1, "for", "(", _, ":", _, ")", maybeContr2, stmnt) => fail(tree, "Unsupported syntax")

    case Jumpstatement0("break", ";") => fail(tree, "Unsupported syntax")
    case Jumpstatement1("continue", ";") => fail(tree, "Unsupported syntax")
    case Jumpstatement2("return", None, ";") => create return_statement()
    case Jumpstatement2("return", Some(returnexpr), ";") => create return_statement(expr(returnexpr))
    case Jumpstatement3("return", initList, ";") => fail(tree, "Unsupported syntax")
    case Jumpstatement4("goto", cppId, ";") => fail(tree, "Unsupported syntax")

    case Declarationstatement0(blockDecl) => convertStatement(blockDecl)
    case Blockdeclaration0(simpledecl) => convertStatement(simpledecl)
    case Blockdeclaration1(asmdef) => fail(tree, "Unsupported syntax")
    case Blockdeclaration2(namespacealiasdef) => fail(tree, "Unsupported syntax")
    case Blockdeclaration3(singdecl) => fail(tree, "Unsupported syntax")
    case Blockdeclaration4(usingdirective) => fail(tree, "Unsupported syntax")
    case Blockdeclaration5(static_assertdecl) => fail(tree, "Unsupported syntax")
    case Blockdeclaration6(aliasdecl) => fail(tree, "Unsupported syntax")
    case Blockdeclaration7(opaqueenumdecl) => fail(tree, "Unsupported syntax")

    case Simpledeclaration0(maybeContract, Some(declSpecSeq), Some(initDeclList), _) => {
        // TODO find out what the contract here is
      val typeofVar = convertDeclspecifierseq(declSpecSeq)
      val result = new VariableDeclaration(typeofVar)
      convertInitdeclaratorlist(initDeclList).foreach{
        case (name, init) =>
          result.add(DeclarationStatement(name, VariableDeclaration.common_type, init))
      }
      result
    }
  }

  def convertInitdeclaratorlist(tree: InitdeclaratorlistContext): Seq[(String, Option[ASTNode])] = tree match {
    case Initdeclaratorlist0(initdecl) => convertInitdeclarator(initdecl)
  }

  def convertInitdeclarator(tree: InitdeclaratorContext): Seq[(String, Option[ASTNode])] = tree match {
    case Initdeclarator0(decl, None) => Seq((convertDeclarator(decl), None))
    case Initdeclarator0(decl, Some(initializer)) => {

//      create assignment (null, null)
      Seq((convertDeclarator(decl), Some(convertInitializer(initializer))))
    }

  }

  def convertInitializer(tree: InitializerContext) = tree match {
    case Initializer0(braeorequal) => convertBraceorequalinitializer(braeorequal)
  }

  def convertBraceorequalinitializer(tree: BraceorequalinitializerContext) = tree match {
    case Braceorequalinitializer0("=", initClause) => convertInitializerclause(initClause)
  }

  def convertInitializerclause(tree: InitializerclauseContext) = tree match {
    case Initializerclause0(assignment) => expr(assignment)
  }

  def convertDeclspecifierseq(tree: DeclspecifierseqContext) = tree match {
    case Declspecifierseq0(declSpec, _) => convertDeclSpecifier(declSpec)
  }

  def convertDeclSpecifier(tree: DeclspecifierContext) = tree match {
    case Declspecifier1(typeSpec) => convertTypespecifier(typeSpec)
  }

  def convertTypespecifier(tree: TypespecifierContext): Type = tree match {
    case Typespecifier0(trailingtype) => convertTrailingtypespecifier(trailingtype)
  }

  def convertTypespecifier(tree: LangTypeContext): Type = tree match {
    case LangType0(t) => convertTypespecifier(t)
  }

  def convertTrailingtypespecifier(tree: TrailingtypespecifierContext): Type = tree match {
    case Trailingtypespecifier0(simp) => convertSimpletypespecifier(simp)
  }

  def convertSimpletypespecifier(tree: SimpletypespecifierContext): Type = tree match {
    case Simpletypespecifier6("bool") => create primitive_type(PrimitiveSort.Boolean)
    case Simpletypespecifier8("int") => create primitive_type(PrimitiveSort.Integer)
    case Simpletypespecifier12("float") => create primitive_type(PrimitiveSort.Float)
    case _ => fail(tree, "Unsupported type")
  }

  def convertDeclarator(tree: DeclaratorContext): String = tree match {
    case Declarator0(ptDecl) => convertPtrdeclarator(ptDecl)
  }

  def convertPtrdeclarator(tree: PtrdeclaratorContext): String  = tree match {
    case Ptrdeclarator0(noptr) => convertNoptrdeclarator(noptr)
//    case Ptrdeclarator1(ptrop, ptrdecl) => null
  }


  def convertNoptrdeclarator(tree: NoptrdeclaratorContext): String  = tree match {
    case Noptrdeclarator0(id, _) => convertDeclaratorid(id)
    case Noptrdeclarator1(noptr, _) => convertNoptrdeclarator(noptr)
  }

  def convertDeclaratorid(tree: DeclaratoridContext) = tree match {
    case Declaratorid0(_, id) => convertIdexpression(id)
  }

  def convertIdexpression(tree: IdexpressionContext) = tree match {
    case Idexpression0(unqual) => convertUnqualifiedid(unqual)
    case Idexpression1(qual) => fail(tree, "Unsupported syntax")
  }

  def convertUnqualifiedid(tree: UnqualifiedidContext) = tree match {
    case Unqualifiedid0(cppId) => convertCppIdentifier(cppId)
  }

  def convertCppIdentifier(tree: CppIdentifierContext): String = tree match {
    case CppIdentifier1(id) => id
  }

  def convertLangId(id: LangIdContext): String = id match {
    case LangId0(id) => convertCppIdentifier(id)
  }

  def convertIDName(id: LangIdContext): NameExpression = id match {
    case LangId0(id) => create unresolved_name convertCppIdentifier(id)
  }

  def convertValExpList(args: ValExpressionListContext): Seq[ASTNode] = args match {
    case ValExpressionList0(exp) =>
      Seq(expr(exp))
    case ValExpressionList1(exp, ",", expList) =>
      expr(exp) +: convertValExpList(expList)
  }

  def convertValClause(clause: ValContractClauseContext) = (builder: ContractBuilder) => clause match {
    case ValContractClause0(_modifies, names, _) =>
      builder.modifies(convertValExpList(names):_*)
    case ValContractClause1(_accessible, names, _) =>
      builder.accesses(convertValExpList(names):_*)
    case ValContractClause2(_requires, exp, _) =>
      builder.requires(expr(exp))
    case ValContractClause3(_ensures, exp, _) =>
      builder.ensures(expr(exp))
    case ValContractClause4(_given, t, name, _) =>
      builder.`given`(create.field_decl(convertLangId(name), convertTypespecifier(t)))
    case ValContractClause5(_yields, t, name, _) =>
      builder.yields(create.field_decl(convertLangId(name), convertTypespecifier(t)))
    case ValContractClause6(_context_everywhere, exp, _) =>
      builder.appendInvariant(expr(exp))
    case ValContractClause7(_context, exp, _) =>
      builder.context(expr(exp))
    case ValContractClause8(_loop_invariant, exp, _) =>
      builder.appendInvariant(expr(exp))
  }

  def convertValBlock(block: ValBlockContext): BlockStatement = origin(block, block match {
    case ValBlock0("{", statements, "}") =>
      create block(statements.map(convertValStat):_*)
  })

  def convertValStat(stat: ValEmbedStatementBlockContext): Seq[ASTNode] = origin(stat, stat match {
    case ValEmbedStatementBlock0(_startSpec, stats, _endSpec) =>
      stats.map(convertValStat)
    case ValEmbedStatementBlock1(stats) =>
      stats.map(convertValStat)
  })

  def convertValStat(stat: LangStatementContext): Seq[ASTNode] = origin(stat, stat match {
    case LangStatement0(stat) =>
      Seq(convertStatement(stat))
  })

  def convertValStat(stat: ValStatementContext): ASTNode = origin(stat, stat match {
    case ValStatement0(_create, block) =>
      create lemma(convertValBlock(block))
    case ValStatement1(_qed, exp, _) =>
      create special(ASTSpecial.Kind.QED, expr(exp))
    case ValStatement2(_apply, exp, _) =>
      create special(ASTSpecial.Kind.Apply, expr(exp))
    case ValStatement3(_use, exp, _) =>
      create special(ASTSpecial.Kind.Use, expr(exp))
    case ValStatement4(_create, hist, _) =>
      create special(ASTSpecial.Kind.CreateHistory, expr(hist))
    case ValStatement5(_create, fut, _, proc, _) =>
      create special(ASTSpecial.Kind.CreateFuture, expr(fut), expr(proc))
    case ValStatement6(_destroy, hist, _, proc, _) =>
      create special(ASTSpecial.Kind.DestroyHistory, expr(hist), expr(proc))
    case ValStatement7(_destroy, fut, _) =>
      create special(ASTSpecial.Kind.DestroyFuture, expr(fut))
    case ValStatement8(_split, fut, _, perm1, _, proc1, _, perm2, _, proc2, _) =>
      create special(ASTSpecial.Kind.SplitHistory, expr(fut), expr(perm1), expr(proc1), expr(perm2), expr(proc2))
    case ValStatement9(_merge, fut, _, perm1, _, proc1, _, perm2, _, proc2, _) =>
      create special(ASTSpecial.Kind.MergeHistory, expr(fut), expr(perm1), expr(proc1), expr(perm2), expr(proc2))
    case ValStatement10(_choose, fut, _, perm, _, proc1, _, proc2, _) =>
      create special(ASTSpecial.Kind.ChooseHistory, expr(fut), expr(perm), expr(proc1), expr(proc2))
    case ValStatement11(_fold, pred, _) =>
      create special(ASTSpecial.Kind.Fold, expr(pred))
    case ValStatement12(_unfold, pred, _) =>
      create special(ASTSpecial.Kind.Unfold, expr(pred))
    case ValStatement13(_open, pred, _) =>
      create special(ASTSpecial.Kind.Open, expr(pred))
    case ValStatement14(_close, pred, _) =>
      create special(ASTSpecial.Kind.Close, expr(pred))
    case ValStatement15(_assert, assn, _) =>
      create special(ASTSpecial.Kind.Assert, expr(assn))
    case ValStatement16(_assume, assn, _) =>
      create special(ASTSpecial.Kind.Assume, expr(assn))
    case ValStatement17(_inhale, res, _) =>
      create special(ASTSpecial.Kind.Inhale, expr(res))
    case ValStatement18(_exhale, res, _) =>
      create special(ASTSpecial.Kind.Exhale, expr(res))
    case ValStatement19(_label, lbl, _) =>
      create special(ASTSpecial.Kind.Label, convertIDName(lbl))
    case ValStatement20(_refute, assn, _) =>
      create special(ASTSpecial.Kind.Refute, expr(assn))
    case ValStatement21(_witness, pred, _) =>
      create special(ASTSpecial.Kind.Witness, expr(pred))
    case ValStatement22(_ghost, code) =>
      flattenIfSingleStatement(convertValStat(code))
    case ValStatement23(_send, res, _to, lbl, _, thing, _) =>
      create special(ASTSpecial.Kind.Send, expr(res), convertIDName(lbl), expr(thing))
    case ValStatement24(_recv, res, _from, lbl, _, thing, _) =>
      create special(ASTSpecial.Kind.Recv, expr(res), convertIDName(lbl), expr(thing))
    case ValStatement25(_transfer, exp, _) =>
      ??(stat)
    case ValStatement26(_csl_subject, obj, _) =>
      create special(ASTSpecial.Kind.CSLSubject, expr(obj))
    case ValStatement27(_spec_ignore, "}") =>
      create special ASTSpecial.Kind.SpecIgnoreEnd
    case ValStatement28(_spec_ignore, "{") =>
      create special ASTSpecial.Kind.SpecIgnoreStart
    case ValStatement29(_action, arg1, _, arg2, _, arg3, _, arg4, map, _) =>
      if(map.nonEmpty) {
        ??(map.head)
      }
      create special (ASTSpecial.Kind.ActionHeader, expr(arg1), expr(arg2), expr(arg3), expr(arg4))
    case ValStatement30(_atomic, _, resList, _, stat) =>
      create csl_atomic(create block(convertValStat(stat):_*), resList.map(convertValExpList).getOrElse(Seq()).map {
        case name: NameExpression if name.getKind == NameExpressionKind.Unresolved =>
          create label name.getName
        case other => other
      }:_*)
  })

  def valExpr(exp: ValPrimaryContext): ASTNode = origin(exp, exp match {
    case ValPrimary0(t, "{", maybeExps, "}") =>
      val exps = maybeExps.map(convertValExpList).getOrElse(Seq())
      create struct_value(convertTypespecifier(t), null, exps:_*)
    case ValPrimary1("[", factor, "]", exp) =>
      create expression(Scale, expr(factor), expr(exp))
    case ValPrimary2("|", seq, "|") =>
      create expression(Size, expr(seq))
    case ValPrimary3("\\unfolding", pred, "\\in", exp) =>
      create expression(Unfolding, expr(pred), expr(exp))
    case ValPrimary4("(", exp, "!", indepOf, ")") =>
      create expression(IndependentOf, expr(exp), convertIDName(indepOf))
    case ValPrimary5("(", x, "\\memberof", xs, ")") =>
      create expression(Member, expr(x), expr(xs))
    case ValPrimary6("{", from, "..", to, "}") =>
      create expression(RangeSeq, expr(from), expr(to))
    case ValPrimary7("*") =>
      create reserved_name ASTReserved.Any
    case ValPrimary8("\\current_thread") =>
      create reserved_name ASTReserved.CurrentThread
    case ValPrimary9(_, binderName, t, id, "=", fr, "..", to, _, main, _) =>
      val name = convertLangId(id)
      val decl = create field_decl(name, convertTypespecifier(t))
      val guard = create expression(StandardOperator.And,
        create expression(LTE, expr(fr), create unresolved_name(name)),
        create expression(StandardOperator.LT, create unresolved_name(name), expr(to))
      )
      binderName match {
        case "\\forall*" => create starall(guard, expr(main), decl)
        case "\\forall" => create forall(guard, expr(main), decl)
        case "\\exists" => create exists(guard, expr(main), decl)
      }
    case ValPrimary10(_, binderName, t, id, _, guard, _, main, _) =>
      val decl = create field_decl(convertLangId(id), convertTypespecifier(t))
      binderName match {
        case "\\forall*" => create starall(expr(guard), expr(main), decl)
        case "\\forall" => create forall(expr(guard), expr(main), decl)
        case "\\exists" => create exists(expr(guard), expr(main), decl)
      }
    case ValPrimary11(_, "\\let", t, id, "=", exp, _, body, _) =>
      create let_expr(create field_decl(convertLangId(id), convertTypespecifier(t), expr(exp)), expr(body))
    case ValPrimary12(_, "\\sum", t, id, _, guard, _, main, _) =>
      create summation(expr(guard), expr(main), create field_decl(convertLangId(id), convertTypespecifier(t)))
    case ValPrimary13("\\length", "(", exp, ")") =>
      create expression(Length, expr(exp))
    case ValPrimary14("\\old", "(", exp, ")") =>
      create expression(Old, expr(exp))
    case ValPrimary15("\\id", "(", exp, ")") =>
      create expression(Identity, expr(exp))
    case ValPrimary16("\\typeof", "(", exp, ")") =>
      create expression(TypeOf, expr(exp))
    case ValPrimary17("\\matrix", "(", m, _, size0, _, size1, ")") =>
      create expression(ValidMatrix, expr(m), expr(size0), expr(size1))
    case ValPrimary18("\\array", "(", a, _, size0, ")") =>
      create expression(ValidArray, expr(a), expr(size0))
    case ValPrimary19("\\pointer", "(", p, _, size0, _, perm, ")") =>
      create expression(ValidPointer, expr(p), expr(size0), expr(perm))
    case ValPrimary20("\\pointer_index", "(", p, _, idx, _, perm, ")") =>
      create expression(ValidPointerIndex, expr(p), expr(idx), expr(perm))
    case ValPrimary21("\\values", "(", a, _, fr, _, to, ")") =>
      create expression(Values, expr(a), expr(fr), expr(to))
    case ValPrimary22("\\sum", "(", a, _, b, ")") =>
      create expression(FoldPlus, expr(a), expr(b))
    case ValPrimary23("\\vcmp", "(", a, _, b, ")") =>
      create expression(VectorCompare, expr(a), expr(b))
    case ValPrimary24("\\vrep", "(", v, ")") =>
      create expression(VectorRepeat, expr(v))
    case ValPrimary25("\\msum", "(", a, _, b, ")") =>
      create expression(MatrixSum, expr(a), expr(b))
    case ValPrimary26("\\mcmp", "(", a, _, b, ")") =>
      create expression(MatrixCompare, expr(a), expr(b))
    case ValPrimary27("\\mrep", "(", m, ")") =>
      create expression(MatrixRepeat, expr(m))
    case ValPrimary28("Reducible", "(", exp, _, opNode, ")") =>
      val opText = opNode match {
        case ValReducibleOperator0("+") => "+"
        case ValReducibleOperator1(id) => convertLangId(id)
      }
      create expression(opText match {
        case "+" => ReducibleSum
        case "min" => ReducibleMin
        case "max" => ReducibleMax
      }, expr(exp))
    case ValPrimary29(label, _, exp) =>
      val res = expr(exp)
      res.addLabel(create label(convertLangId(label)))
      res
  })

  def convertValOp(op: ValImpOpContext): StandardOperator = op match {
    case ValImpOp0("-*") => StandardOperator.Wand
    case ValImpOp1("==>") => StandardOperator.Implies
  }

  def convertValOp(op: ValAndOpContext): StandardOperator = op match {
    case ValAndOp0("**") => StandardOperator.Star
  }

  def convertValOp(op: ValMulOpContext): StandardOperator = op match {
    case ValMulOp0("\\") => StandardOperator.Div
  }

  def convertValReserved(reserved: ValReservedContext): NameExpression = origin(reserved, reserved match {
    case ValReserved0(_) =>
      fail(reserved, "This identifier is reserved and cannot be declared or used.")
    case ValReserved1("\\result") =>
      create reserved_name ASTReserved.Result
    case ValReserved2("\\current_thread") =>
      create reserved_name ASTReserved.CurrentThread
    case ValReserved3("none") =>
      create reserved_name ASTReserved.NoPerm
    case ValReserved4("write") =>
      create reserved_name ASTReserved.FullPerm
    case ValReserved5("read") =>
      create reserved_name ASTReserved.ReadPerm
    case ValReserved6("None") =>
      create reserved_name ASTReserved.OptionNone
    case ValReserved7("empty") =>
      create reserved_name ASTReserved.EmptyProcess
  })

  /**
   * This method allows a language grammar to step into the reserved identifiers where they overlap with the underlying
   * language, to allow their use there. They should be forbidden inside specifications.
   * @param reserved the reserved identifier
   * @return the string representation of the identifier
   */
  def convertOverlappingValReservedID(reserved: ValReservedContext): String = reserved match {
    case ValReserved0(s) => s
    case ValReserved1("\\result") => fail(reserved, "This identifier is invalid in the current language")
    case ValReserved2("\\current_thread") => fail(reserved, "This identifier is invalid in the current language")
    case ValReserved3(s) => s
    case ValReserved4(s) => s
    case ValReserved5(s) => s
    case ValReserved6(s) => s
    case ValReserved7(s) => s
  }

  def convertOverlappingValReservedName(reserved: ValReservedContext): NameExpression =
    create unresolved_name convertOverlappingValReservedID(reserved)

  def convertValContract(contract: Option[ValEmbedContractContext]) = (builder: ContractBuilder) => contract match {
    case Some(ValEmbedContract0(blocks)) =>
      for(block <- blocks) {
        convertValContractBlock(block)(builder)
      }
    case None =>
    // nop
  }

  def convertValContractBlock(contract: ValEmbedContractBlockContext) = (builder: ContractBuilder) => contract match {
    case ValEmbedContractBlock0(_startSpec, clauses, _endSpec) =>
      for(clause <- clauses) {
        convertValClause(clause)(builder)
      }
    case ValEmbedContractBlock1(clauses) =>
      for(clause <- clauses) {
        convertValClause(clause)(builder)
      }
  }

  def convertValType(t: ValTypeContext): Type = origin(t, t match {
    case ValType0(s) => s match {
      case "resource" => create primitive_type(PrimitiveSort.Resource)
      case "process" => create primitive_type(PrimitiveSort.Process)
      case "frac" => create primitive_type PrimitiveSort.Fraction
      case "zfrac" => create primitive_type PrimitiveSort.ZFraction
      case "rational" => create primitive_type PrimitiveSort.Rational
      case "bool" => create primitive_type PrimitiveSort.Boolean
    }
    case ValType1("seq", _, subType, _) =>
      create primitive_type(PrimitiveSort.Sequence, convertTypespecifier(subType))
  })

  def convertValArg(arg: ValArgContext): DeclarationStatement = origin(arg, arg match {
    case ValArg0(t, id) =>
      create field_decl(convertLangId(id), convertTypespecifier(t))
  })

  def convertValArgList(argList: ValArgListContext): Seq[DeclarationStatement] = origin(argList, argList match {
    case ValArgList0(arg) => Seq(convertValArg(arg))
    case ValArgList1(arg, _, args) => convertValArg(arg) +: convertValArgList(args)
  })

  def convertValModifier(modifier: ValModifierContext): NameExpression = origin(modifier, modifier match {
    case ValModifier0(s) => s match {
      case "pure" => create reserved_name(ASTReserved.Pure)
      case "inline" => create reserved_name(ASTReserved.Inline)
      case "thread_local" => create reserved_name(ASTReserved.ThreadLocal)
    }
    case ValModifier1(langMod) => convertModifier(langMod)
  })

  def convertValModifiers(modifiers: ValEmbedModifiersContext): Seq[NameExpression] = origin(modifiers, modifiers match {
    case ValEmbedModifiers0(_, mods, _) =>
      mods.map(convertValModifier)
    case ValEmbedModifiers1(mods) =>
      mods.map(convertValModifier)
  })

  def convertValDecl(decl: ValDeclarationContext): ASTDeclaration = origin(decl, decl match {
    case ValDeclaration0(clauses, mods, t, name, _, args, _, body) =>
      val contract = getContract(clauses.map(convertValClause):_*)
      val func = create function_decl(
        convertTypespecifier(t),
        contract,
        convertLangId(name),
        args.map(convertValArgList).getOrElse(Seq()).toArray,
        body match {
          case ValPredicateDef0(_) => null
          case ValPredicateDef1("=", exp, _) => expr(exp)
        }
      )
      mods.foreach(mod => func.attach(convertValModifier(mod)))
      func
    case ValDeclaration1("axiom", name, _, left, "==", right, _) =>
      create axiom(convertLangId(name), create expression(EQ, expr(left), expr(right)))
    case ValDeclaration2(clauses, "ghost", langDecl) =>
      val decl = convertDeclaration(langDecl)
      if(clauses.nonEmpty) {
        decl match {
          case method: Method =>
            method.setContract(getContract(clauses.map(convertValClause):_*))
            method
          case _ =>
            fail(langDecl, "This constructor cannot have contract declarations")
        }
      } else {
        decl
      }
  })

  def convertValDecl(decl: ValEmbedDeclarationBlockContext): Seq[ASTDeclaration] = decl match {
    case ValEmbedDeclarationBlock0(_, decls, _) =>
      decls.map((decl) => convertValDecl(decl))
  }

  def convertDeclaration(tree: LangDeclContext): ASTDeclaration = tree match {
    case LangDecl0(decl) => convertDeclaration(decl)
  }

  def convertValWithThen(withThen: ValWithThenContext): ASTNode = withThen match {
    case ValWithThen0("with", stat) =>
      create special(ASTSpecial.Kind.With, flattenIfSingleStatement(convertValStat(stat)))
    case ValWithThen1("then", stat) =>
      create special(ASTSpecial.Kind.Then, flattenIfSingleStatement(convertValStat(stat)))
  }

  def convertValWithThen(withThen: ValEmbedWithThenBlockContext): Seq[ASTNode] = withThen match {
    case ValEmbedWithThenBlock0(_, mappings, _) => mappings.map(convertValWithThen)
    case ValEmbedWithThenBlock1(mappings) => mappings.map(convertValWithThen)
  }

  def convertValWithThen(withThen: ValEmbedWithThenContext): Seq[ASTNode] = withThen match {
    case ValEmbedWithThen0(blocks) => blocks.flatMap(convertValWithThen)
  }

  def convertModifier(mod: LangModifierContext): NameExpression = ???

  def expr(exp: LangExprContext): ASTNode = exp match {
    case LangExpr0(e) => expr(e)
  }

}