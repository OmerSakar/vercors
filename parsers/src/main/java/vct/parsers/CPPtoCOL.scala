package vct.parsers

import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext}
import vct.antlr4.generated.CPPParser
import vct.antlr4.generated.CPPParser._
import vct.antlr4.generated.CPPParserPatterns._
import vct.antlr4.generated.JavaParserPatterns.BlockStatement0
import vct.col.ast.`type`.{PrimitiveSort, Type}
import vct.col.ast.expr.StandardOperator
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.Method.Kind
import vct.col.ast.stmt.decl.{ASTDeclaration, DeclarationStatement, ProgramUnit, VariableDeclaration}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.ContractBuilder


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

  def convertFunctiondefinition(tree: FunctiondefinitionContext): ASTDeclaration = tree match {
    case Functiondefinition0(maybeAttrSeq, maybeDeclSpecSeq, declarator, maybeVirtSpecSeq, funcBody) => {
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
      //TODO contract, arguments, kind, declspecseq(which can contain the kind etc)
      create.method_kind(Kind.Plain, declspecseq, null /*TODO contract*/,
        name, Seq.empty[DeclarationStatement].toArray, body)//maybeArgs.map(convertArgs).getOrElse(Seq()).toArray, body.orNull)
    }
  }

  def expr(tree: ParserRuleContext): ASTNode = tree match {
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
    case Relationalexpression1(lft, ">", rght) => create expression(StandardOperator.GT, expr(lft), expr(rght))
    case Relationalexpression1(lft, "<=", rght) => create expression(StandardOperator.LTE, expr(lft), expr(rght))
    case Relationalexpression1(lft, ">=", rght) => create expression(StandardOperator.GTE, expr(lft), expr(rght))

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
    case Primaryexpression3(idexpr) => create unresolved_name convertIdexpression(idexpr)

    case Literal0(integer) => create constant Integer.parseInt(integer)
    case Literal4(bool) => bool match {
      case Booleanliteral0("false") => create constant(false)
      case Booleanliteral1("true") => create constant(true)
    }

    case Condition0(condexpr) => expr(condexpr)
  }

  def convertFunctionbody(tree: FunctionbodyContext): BlockStatement= tree match {
    case Functionbody0(_, Compoundstatement0("{", None, "}")) => new BlockStatement() //TODO what was the ASTNode for a body
    case Functionbody0(_, Compoundstatement0("{", Some(stmntSeq), "}")) => {
      val block = new BlockStatement()
      convertStatementseq(stmntSeq).foreach(block.add)
      block
    }
  }

  def convertStatementseq(tree: StatementseqContext): Seq[ASTNode] = tree match {
    case Statementseq0(stmnt) => Seq(convertStatement(stmnt))
    case Statementseq1(stmntseq, stmnt) => convertStatementseq(stmntseq) :+ convertStatement(stmnt)
  }

  def convertStatement(tree: ParserRuleContext): ASTNode = tree match {
    case Statement0(labeledStmnt) => convertStatement(labeledStmnt)
    case Statement1(_, exprStmnt) => convertStatement(exprStmnt)
    case Statement2(_, compoundStmnt) => convertStatement(compoundStmnt)
    case Statement3(_, selectionStmnt) => convertStatement(selectionStmnt)
    case Statement4(_, iterationStmnt) => convertStatement(iterationStmnt)
    case Statement5(_, jumpStmnt) => convertStatement(jumpStmnt)
    case Statement6(declStmnt) => convertStatement(declStmnt)
    case Statement7(_, tryblock) => convertStatement(tryblock)

    case Labeledstatement0(_) => fail(tree, "Unsupported syntax")
    case Labeledstatement1(_) => fail(tree, "Unsupported syntax")
    case Labeledstatement2(_) => fail(tree, "Unsupported syntax")

    case Expressionstatement0(None, _) => new BlockStatement
    case Expressionstatement0(Some(exprstmnt), _) => expr(exprstmnt)

    case Compoundstatement0(_, None, _) => new BlockStatement
    case Compoundstatement0(_, Some(stmntSeq), _) =>
      convertStatementseq(stmntSeq).foldLeft(new BlockStatement) {(block, next) => block.add(next) }


    case Selectionstatement0("if", "(", condition, ")", ifstmnt) => {
      create ifthenelse(expr(condition), convertStatement(ifstmnt), null)
    }
    case Selectionstatement1("if", "(", condition, ")", ifstmnt, "else", elsestmnt) => {
      create ifthenelse(expr(condition), convertStatement(ifstmnt), convertStatement(elsestmnt))
    }
    case Selectionstatement2("switch", "(", condition, ")", switchstmnt) => fail(tree, "Unsupported syntax")

    case Iterationstatement0("while", "(", condition, ")", stmnt) => {
      create while_loop(expr(condition), convertStatement(stmnt), new ContractBuilder().getContract(false)/*TODO invariants*/)
    }
    case Iterationstatement1("do", stmnt, "while", "(", expr, ")", _) => fail(tree, "Unsupported syntax")
    case Iterationstatement2("for", "(", _, _, ";", _, ")", stmnt) => fail(tree, "Unsupported syntax")
    case Iterationstatement3("for", "(", _, ":", _, ")", stmnt) => fail(tree, "Unsupported syntax")

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

    case Simpledeclaration0(Some(declSpecSeq), Some(initDeclList), _) => {
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
    case Declspecifier1(typeSpec) => convertTypecpecifier(typeSpec)
  }

  def convertTypecpecifier(tree: TypespecifierContext): Type = tree match {
    case Typespecifier0(trailingtype) => convertTrailingtypespecifier(trailingtype)
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

}