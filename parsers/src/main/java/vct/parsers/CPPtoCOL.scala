//package vct.parsers
//
//import org.antlr.v4.runtime.{CommonTokenStream, ParserRuleContext}
//import vct.antlr4.generated.CPPParser
//import vct.antlr4.generated.CPPParser._
//import vct.antlr4.generated.CPPParserPatterns._
//import vct.col.ast.`type`.{ASTReserved, ClassType, PrimitiveSort, Type}
//import vct.col.ast.expr.{NameExpression, NameExpressionKind, StandardOperator}
//import vct.col.ast.generic.ASTNode
//import vct.col.ast.stmt.composite.BlockStatement
//import vct.col.ast.stmt.decl.Method.Kind
//import vct.col.ast.stmt.decl.{ASTClass, ASTDeclaration, ASTSpecial, DeclarationStatement, Method, NameSpace, ProgramUnit, VariableDeclaration}
//import vct.col.ast.stmt.decl.ASTClass.ClassKind
//import vct.col.ast.util.ContractBuilder
//import vct.col.ast.expr.StandardOperator._
//import vct.col.ast.langspecific.c.{CFunctionType, ParamSpec}
//
//import scala.collection.immutable.{Bag, HashedBagConfiguration}
//import scala.collection.mutable
//
//
//object CPPtoCOL {
//  def convert(tree: TranslationunitContext, fileName: String, tokens: CommonTokenStream, parser: CPPParser): ProgramUnit = {
//    val result = CPPtoCOL(fileName, tokens, parser).convertProgram(tree)
//    result
//  }
//}
//
//case class CPPtoCOL(fileName: String, tokens: CommonTokenStream, parser: CPPParser)
//  extends ToCOL(fileName, tokens, parser) {
//
//  def convertProgram(tree: TranslationunitContext): ProgramUnit = tree match {
//    case Translationunit0(maybeDeclseq, _) => {
//      val pu = new ProgramUnit()
//      maybeDeclseq.map(convertDeclarationseq).getOrElse(Seq()).foreach(pu.add)
//      pu
//    }
//  }
//
//  def convertDeclarationseq(tree: DeclarationseqContext): Seq[ASTNode] = tree match {
//    case Declarationseq0(decl) => Seq(convertDeclaration(decl))
//    case Declarationseq1(declseq, decl) => convertDeclarationseq(declseq) :+ convertDeclaration(decl)
//  }
//
//  def convertDeclaration(tree: DeclarationContext): ASTNode = tree match {
//    case Declaration0(block) => convertBlockdeclaration(block)
//    case Declaration1(func) => {
//      val function = convertFunctiondefinition(func)
//      function.setStatic(true)
//      function
//    }
//    case Declaration7(emptyDecl) => new BlockStatement
//    case _ => ??(tree)
//  }
//
//  def convertBlockdeclaration(tree: BlockdeclarationContext): ASTNode = tree match {
//    case Blockdeclaration0(simpledecl) => convertStatement(simpledecl)
//    case Blockdeclaration4(usingdir) => convertUsingdirective(usingdir)
//    case _ => ??(tree)
//  }
//
//  def convertDeclarationstatement(tree: DeclarationstatementContext): ASTNode = tree match {
//    case Declarationstatement0(blockDecl) => convertBlockdeclaration(blockDecl)
//  }
//
//  def convertInitdeclaratorlist(tree: InitdeclaratorlistContext): Seq[(String, Option[ASTNode])] = tree match {
//    case Initdeclaratorlist0(initdecl) => Seq(convertInitdeclarator(initdecl))
//    case Initdeclaratorlist1(decllist, ",", initdecl) => convertInitdeclaratorlist(decllist) :+ convertInitdeclarator(initdecl)
//  }
//
//  def convertInitdeclarator(tree: InitdeclaratorContext): (String, Option[ASTNode]) = tree match {
//    case Initdeclarator0(decl, maybeInit) => (convertDeclaratorName(decl), maybeInit.map(convertInitializer))
//  }
//
//  def convertInitializer(tree: InitializerContext) = tree match {
//    case Initializer0(braeorequal) => convertBraceorequalinitializer(braeorequal)
//    case _ => ??(tree)
//  }
//
//  def convertBraceorequalinitializer(tree: BraceorequalinitializerContext) = tree match {
//    case Braceorequalinitializer0("=", initClause) => convertInitializerclause(initClause)
//    case _ => ??(tree)
//  }
//
//  def convertExpressionlist(tree: ExpressionlistContext) = origin(tree, tree match {
//    case Expressionlist0(initlist) => convertInitializerlist(initlist)
//  })
//
//  def convertInitializerlist(tree: InitializerlistContext): Seq[ASTNode] = tree match {
//    // The _ here is the ... operator. I dont think we have support for this, but we can represent it as an array/seq
//    case Initializerlist0(initializerclause, _) => Seq(convertInitializerclause(initializerclause))
//    case Initializerlist1(initlist, _, initclause, _) => convertInitializerlist(initlist) :+ convertInitializerclause(initclause)
//  }
//
//  def convertInitializerclause(tree: InitializerclauseContext) = tree match {
//    case Initializerclause0(assignment) => expr(assignment)
////    case Initializerclause1(bracedinit) => {
////
////            create expression(OptionSome,
////              create struct_value(tArray(tCell(addDims(baseType, 1))), null, convertBracedinitlist(bracedinit):_*))
////    }
//    case _ => ??(tree)
//  }
//
//  def convertBracedinitlist(tree: BracedinitlistContext): Seq[ASTNode] = tree match {
//    case Bracedinitlist0("{", initlist, _, "}") => convertInitializerlist(initlist)
//    case Bracedinitlist1("{", "}") => Seq()
//  }
//
//  def convertSimpletemplateid(tree: SimpletemplateidContext): ClassType = origin(tree, tree match {
//    case Simpletemplateid0(templname, _, templarglist, _) => {
//      val classArgs = templarglist match {
//        case None => Seq.empty[Type]
//        case Some(lst) => convertTemplateargumentlist(lst)
//      }
//      val newClasstype = create.class_type(convertTemplatename(templname), classArgs: _*)
//      newClasstype
//    }
//  })
//
//  def convertTemplateargumentlist(tree: TemplateargumentlistContext): Seq[ASTNode] = origin(tree, tree match {
//    // The _ here is the ... operator. I dont think we have support for this, but we can represent it as an array/seq
//    case Templateargumentlist0(templarg, _) => Seq(convertTemplateargument(templarg))
//    case Templateargumentlist1(templarglist, _, templarg, _) => convertTemplateargumentlist(templarglist) :+ convertTemplateargument(templarg)
//  })
//
//  def convertTemplateargument(tree: TemplateargumentContext): ASTNode = origin(tree, tree match {
//    case Templateargument0(thetypeid) => convertThetypeid(thetypeid)
//    case Templateargument1(constantexpression) => expr(constantexpression)
//    case Templateargument2(idexpr) => ??(tree)
//  })
//
//  def convertTemplatename(tree: TemplatenameContext): String = tree match {
//    case Templatename0(cppid) => convertCppIdentifier(cppid).name
//  }
//
//
//  class DeclSpecs {
//
//    sealed trait TypeSpec
//
//    case class PrimitiveTypeSpec(primitive: String) extends TypeSpec
//
//    case class ClassTypeSpec(clazz: ClassType) extends TypeSpec
//
//    case class TypedefNameTypeSpec(name: String) extends TypeSpec
//
//    case class ValTypeSpec(t: Type) extends TypeSpec
//
//    // Scala magic that can be safely ignored: needed to use bags ("multisets")
//    private implicit val m1: HashedBagConfiguration[PrimitiveTypeSpec] = Bag.configuration.compact[PrimitiveTypeSpec]
//    private implicit val m2: HashedBagConfiguration[TypeSpec] = Bag.configuration.compact[TypeSpec]
//    private implicit val m3: mutable.HashedBagConfiguration[TypeSpec] = mutable.Bag.configuration.compact[TypeSpec]
//
//    val primitiveTypeSets: Map[Bag[TypeSpec], PrimitiveSort] = Map(
//      Bag[TypeSpec](PrimitiveTypeSpec("void"))
//        -> PrimitiveSort.Void,
//      Bag[TypeSpec](PrimitiveTypeSpec("char"))
//        -> PrimitiveSort.Char,
//      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("char"))
//        -> PrimitiveSort.Char,
//      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("char"))
//        -> PrimitiveSort.Char,
//      Bag[TypeSpec](PrimitiveTypeSpec("short"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("short"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("short"), PrimitiveTypeSpec("int"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("short"), PrimitiveTypeSpec("int"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("short"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("short"), PrimitiveTypeSpec("int"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("int"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("signed"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("int"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("int"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("long"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("long"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("long"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("signed"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("unsigned"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("long"), PrimitiveTypeSpec("int"))
//        -> PrimitiveSort.Integer,
//      Bag[TypeSpec](PrimitiveTypeSpec("float"))
//        -> PrimitiveSort.Float,
//      Bag[TypeSpec](PrimitiveTypeSpec("double"))
//        -> PrimitiveSort.Float,
//      Bag[TypeSpec](PrimitiveTypeSpec("long"), PrimitiveTypeSpec("double"))
//        -> PrimitiveSort.Float,
//      Bag[TypeSpec](PrimitiveTypeSpec("bool"))
//        -> PrimitiveSort.Boolean,
//    )
//
//
//    sealed trait FuncSpec
//
//    object InlineFuncSpec extends FuncSpec
//
//    object VirtualFuncSpec extends FuncSpec
//
//    object ExplicitFuncSpec extends FuncSpec
//
//    sealed trait StorageClass
//
//    //    object Typedef extends StorageClass
//    //    object ExternSC extends StorageClass
//    //    object Auto extends StorageClass
//
//    object ExternSC extends StorageClass
//
//    object MutableSC extends StorageClass
//
//    object StaticSC extends StorageClass
//
//    object ThreadLocalSC extends StorageClass
//
//    object RegisterSC extends StorageClass
//
//
//    private val _typeSpec: mutable.Bag[TypeSpec] = mutable.Bag()
//    private val _funcSpec: mutable.Set[FuncSpec] = mutable.Set()
//    private var _storageClass: Option[StorageClass] = None
//    private var _decls: mutable.Seq[ASTDeclaration] = mutable.Seq()
//
//    var valModifiers: mutable.Seq[NameExpression] = mutable.Seq()
//
//    def typeSpec: Bag[TypeSpec] = Bag(_typeSpec.toSeq: _*)
//
//    def funcSpec: Set[FuncSpec] = _funcSpec.toSet
//
//    def decls = _decls.toSet
//
//    def storageClass: Option[StorageClass] = _storageClass
//
//    def add(tree: DeclspecifierseqContext): Unit = tree match {
//      case Declspecifierseq0(declSpec, _) => add(declSpec)
//      case Declspecifierseq1(declSpec, declspeclist) => {
//        add(declspeclist)
//        add(declSpec)
//      }
//    }
//
//    def add(tree: DeclspecifierContext): Unit = tree match {
//      case Declspecifier0(storageclassspec) => storageclassspec match {
//        case Storageclassspecifier0(register) => _storageClass = Some(RegisterSC)
//        case Storageclassspecifier1(static) => _storageClass = Some(StaticSC)
//        case Storageclassspecifier2(tl) => _storageClass = Some(ThreadLocalSC)
//        case Storageclassspecifier3(extern) => _storageClass = Some(ExternSC)
//        case Storageclassspecifier4(mutable) => _storageClass = Some(MutableSC)
//      }
//      case Declspecifier1(typespec) => add(typespec)
//      case Declspecifier2(functionspec) => functionspec match {
//        case Functionspecifier0(inline) => _funcSpec += InlineFuncSpec
//        case Functionspecifier0(virtual) => _funcSpec += VirtualFuncSpec
//        case Functionspecifier0(explicit) => _funcSpec += ExplicitFuncSpec
//      }
//      case Declspecifier3(friend) => ??(tree)
//      case Declspecifier4(typedef) => ??(tree)
//      case Declspecifier5(const) => ??(tree)
//      case Declspecifier6(valEmbedMods) => valModifiers ++= convertValModifiers(valEmbedMods)
//    }
//
//    def add(tree: TypespecifierseqContext): Unit = tree match {
//      case Typespecifierseq0(typespecifier, maybeAttributespecifierseq) => add(typespecifier)
//      case Typespecifierseq1(typespecifier, typespecifierseq) => {
//        add(typespecifier)
//        add(typespecifierseq)
//      }
//    }
//
//    def add(tree: TypespecifierContext): Unit = tree match {
//      case Typespecifier0(trailing) => add(trailing)
//      case Typespecifier1(classspec) => _decls ++= Seq(convertClassspecifier(classspec))
//      case Typespecifier2(enumspec) => ??(enumspec)
//    }
//
//    def add(tree: TrailingtypespecifierContext): Unit = tree match {
//      case Trailingtypespecifier0(simptype) => add(simptype)
//      case Trailingtypespecifier0(simptype) => add(simptype)
//
//      case Trailingtypespecifier1(elaboratedtypespec) => add(elaboratedtypespec)
//      case Trailingtypespecifier2(typenamespec) => ??(tree)
//      case Trailingtypespecifier3(cvqual) => ??(tree)
//    }
//
//    def add(tree: ElaboratedtypespecifierContext): Unit = tree match {
//      case Elaboratedtypespecifier0(classkey, _, maybeNamespec, cppId) => {
//        _decls ++=
//          Seq(create ast_class(
//            maybeNamespec.map(convertNestednamespecifier).getOrElse(Seq()).mkString("", "_", "_") +
//              convertCppIdentifier(cppId),
//            convertClasskey(classkey),
//            Array.empty[DeclarationStatement],
//            Array.empty[ClassType],
//            Array.empty[ClassType]
//          ))
//      }
//      case Elaboratedtypespecifier1(classkey, simpletemplateid) => ??(tree)
//      case Elaboratedtypespecifier2(classkey, nestednamespec, maybeTemplate, simpletempid) => ??(tree)
//      case Elaboratedtypespecifier3(enum, maybeNestednamespec, cppId) => ??(tree)
//
//    }
//
//    def add(tree: SimpletypespecifierContext): Unit = tree match {
//      case Simpletypespecifier0(maybeNested, thetypename) => {
//        val tmpClassType = convertThetypename(thetypename).asInstanceOf[ClassType]
//        val namespace = maybeNested match {
//          case None => ""
//          case Some(namespace) => convertNestednamespecifier(namespace).mkString("", "_", "_")
//        }
//
//        _typeSpec += ClassTypeSpec(create.class_type(namespace + tmpClassType.getName, tmpClassType.params: _*))
//      }
//      case Simpletypespecifier1(_, _, _) => ??(tree)
//      case Simpletypespecifier2(char) => _typeSpec += PrimitiveTypeSpec(char)
//      case Simpletypespecifier3(char16_t) => _typeSpec += PrimitiveTypeSpec("char") //UTF-16
//      case Simpletypespecifier4(char32_t) => _typeSpec += PrimitiveTypeSpec("char") //UTF-32
//      case Simpletypespecifier5(wchar_t) => ??(tree)
//      case Simpletypespecifier6(booltype) => _typeSpec += PrimitiveTypeSpec(booltype)
//      case Simpletypespecifier7(shorttype) => _typeSpec += PrimitiveTypeSpec(shorttype)
//      case Simpletypespecifier8(inttype) => _typeSpec += PrimitiveTypeSpec(inttype)
//      case Simpletypespecifier9(longtype) => _typeSpec += PrimitiveTypeSpec(longtype)
//      case Simpletypespecifier10(signed) => _typeSpec += PrimitiveTypeSpec(signed)
//      case Simpletypespecifier11(unsigned) => _typeSpec += PrimitiveTypeSpec(unsigned)
//      case Simpletypespecifier12(floattype) => _typeSpec += PrimitiveTypeSpec(floattype)
//      case Simpletypespecifier13(doubletype) => _typeSpec += PrimitiveTypeSpec(doubletype)
//      case Simpletypespecifier14(voidtype) => _typeSpec += PrimitiveTypeSpec(voidtype)
//      case Simpletypespecifier15(auto) => ??(tree)
//      case Simpletypespecifier16(decltypespec) => ??(tree)
//      case Simpletypespecifier17(valType) => _typeSpec += ValTypeSpec(convertValType(valType))
//    }
//
//    def add(tree: DeclaratorContext): Unit = tree match {
//      case Declarator0(ptrdecl) =>
//      case Declarator1(noptr, params, _) =>
//    }
//
//
//    def getType: Either[String, Type] = {
//      if (typeSpec.size == 1) {
//        typeSpec.head match {
//          case ValTypeSpec(t) => return Right(t)
//          case ClassTypeSpec(t) => return Right(t)
//          case _ =>
//        }
//      }
//
//      val primitive = primitiveTypeSets.get(typeSpec) match {
//        case None =>
//          return Left("Type specifiers other than primitive types are not supported")
//        case Some(t) =>
//          Right(create primitive_type t)
//      }
//      primitive
//    }
//
//  }
//
//  def convertNestednamespecifier(tree: NestednamespecifierContext): Seq[String] = tree match {
//    case Nestednamespecifier0(_) => Seq()
//    case Nestednamespecifier1(thetypename, _) => convertThetypename(thetypename).asInstanceOf[ClassType].names
//    case Nestednamespecifier2(namespacename, _) => ??(tree)
//    case Nestednamespecifier3(decltypespec, _) => ??(tree)
//    case Nestednamespecifier4(nestednamespec, cppId, _) =>
//      convertNestednamespecifier(nestednamespec) :+ convertCppIdentifier(cppId).name
//    case Nestednamespecifier5(nestednamespecifier, _, simpletemplateid, _) => ??(tree)
//
//  }
//
//  def convertNamespacename(tree: NamespacenameContext): String = tree match {
//    case Namespacename0(Originalnamespacename0(cppid)) => convertCppIdentifier(cppid).name
//    case Namespacename1(Namespacealias0(cppid)) => convertCppIdentifier(cppid).name
//  }
//
//  //TODO check if the method below can be deleted
//  def convertDeclaratorName(tree: DeclaratorContext): String = tree match {
//    case Declarator0(ptrdecl) => convertPtrdeclarator(ptrdecl)
//    case Declarator1(noptr, params, trailingreturntype) => convertNoptrdeclarator(noptr)
//  }
//
//  def convertDeclaratorName(tree: NoptrdeclaratorContext): String = tree match {
//    case Noptrdeclarator0(id, _) => convertDeclaratorid(id).name
//    case Noptrdeclarator1(noptr, _) => convertDeclaratorName(noptr)
//    case Noptrdeclarator2(noptrdeclarator, "[", params, "]", _) => convertDeclaratorName(noptrdeclarator)
//  }
//
//  def convertPtrdeclarator(tree: PtrdeclaratorContext): String = tree match {
//    case Ptrdeclarator0(noptr) => convertNoptrdeclarator(noptr)
//    case Ptrdeclarator1(ptrop, ptrdecl) => convertPtrdeclarator(ptrdecl)
//  }
//
//
//  def convertNoptrdeclarator(tree: NoptrdeclaratorContext): String = tree match {
//    case Noptrdeclarator0(id, _) => convertDeclaratorid(id).name
//    case Noptrdeclarator1(noptr, _) => convertNoptrdeclarator(noptr)
//    case Noptrdeclarator2(noptrdeclarator, "[", _, "]", _) => convertNoptrdeclarator(noptrdeclarator)
//    //case Noptrdeclarator3(_, ptrdecl, _) => ??(tree)
//  }
//
//
//  def convertDeclaratorid(tree: DeclaratoridContext) = tree match {
//    case Declaratorid0(_, id) => convertIdexpression(id)
//  }
//
//  def convertIdexpression(tree: IdexpressionContext) = tree match {
//    case Idexpression0(unqual) => convertUnqualifiedid(unqual)
//    case Idexpression1(qual) => convertQualifiedid(qual)
//  }
//
//  def convertUnqualifiedid(tree: UnqualifiedidContext) = tree match {
//    case Unqualifiedid0(cppId) => convertCppIdentifier(cppId)
//    case _ => ??(tree)
//  }
//
//  def convertQualifiedid(tree: QualifiedidContext) = origin(tree, tree match {
//    case Qualifiedid0(nestednamespec, maybeTemplate, unqualid) => {
//      create unresolved_name (
//        convertNestednamespecifier(nestednamespec).mkString("", "_", "_") +
//          convertUnqualifiedid(unqualid)
//        )
//    }
//  })
//
//  def convertCppIdentifier(tree: CppIdentifierContext): NameExpression = tree match {
//    case CppIdentifier0(valReserved) => convertValReserved(valReserved)
//    case CppIdentifier1(id) => create unresolved_name id
//    case CppIdentifier2(valReserved) => convertValReserved(valReserved)
//  }
//
//  def convertLangId(id: LangIdContext): String = id match {
//    case LangId0(id) => convertCppIdentifier(id).name
//  }
//
//  def convertIDName(id: LangIdContext): NameExpression = id match {
//    case LangId0(id) => convertCppIdentifier(id)
//  }
//
//  def convertModifier(mod: LangModifierContext): NameExpression = ???
//
//  def convertDeclaration(tree: LangDeclContext): ASTNode = tree match {
//    case LangDecl0(decl) => convertDeclaration(decl)
//  }
//
//  /**
//   * //////////////////////////////////////////
//   * ///////////////// Types //////////////////
//   * //////////////////////////////////////////
//   */
//  //TODO remove this
//  def convertTypespecifier(tree: LangTypeContext): Type = tree match {
//    case LangType0(t) => {
//      val specs = new DeclSpecs
//      specs.add(t)
//      getOrFail(t, specs.getType)
//    }
//  }
//
//
//  def convertThetypename(tree: ThetypenameContext): Type = origin(tree, tree match {
//    case Thetypename0(classname) => convertClassname(classname)
//    case Thetypename1(enumname) => ??(tree)
//    case Thetypename2(typedefname) => convertTypedefname(typedefname)
//    case Thetypename3(simptemplid) => ??(tree)
//  })
//
//  def convertTypedefname(tree: TypedefnameContext) = tree match {
//    case Typedefname0(cppid) => ??(tree)
//  }
//
//  def convertThetypeid(tree: ThetypeidContext) = origin(tree, tree match {
//    case Thetypeid0(tspecseq, _) => {
//      val specs = new DeclSpecs
//      specs.add(tspecseq)
//      getOrFail(tspecseq, specs.getType)
//    }
//  })
//
//  def convertDeclaratorType(decl: DeclaratorContext): (Type => Type) = decl match {
//    case Declarator0(ptrdecl) => convertDeclaratorType(ptrdecl)
//    case Declarator1(noptrdecl, params, _) =>
//      t => convertDeclaratorType(noptrdecl)(CFunctionType(convertParametersandqualifiers(params), t))
//  }
//
//  def convertUsingdirective(tree: UsingdirectiveContext): NameSpace = tree match {
//    case Usingdirective0(maybeAttrspecseq, _, _, maybeNestednamespec, namespacename, _) => {
//      val ns = maybeNestednamespec.map(convertNestednamespecifier).getOrElse(Seq()) :+
//        convertNamespacename(namespacename)
//      create namespace (ns: _*)
//    }
//  }
//
//
//  def convertDeclaratorType(decl: PtrdeclaratorContext): (Type => Type) = decl match {
//    case Ptrdeclarator0(noptrdecl) => convertDeclaratorType(noptrdecl)
//    case Ptrdeclarator1(ptrop, ptrdecl) =>
//      //TODO the lambda below might be incorrect.
//      t => (convertDeclaratorType(ptrdecl)(convertPtroperator(ptrop)(t)))
//  }
//
//
//  def convertPtroperator(tree: PtroperatorContext): (Type => Type) = tree match {
//    case Ptroperator0("*", maybeAttrspecseq, maybeCvqual) => {
//      failIfDefined(maybeCvqual, "Qualifiers to pointers such as const are not supported")
//      (t => create primitive_type(PrimitiveSort.Pointer, t))
//    }
//    case Ptroperator1("&", maybeAttrspecseq) => ??(tree)
//    case Ptroperator2("&&", maybeAttrspecseq) => ??(tree)
//    case Ptroperator3(_, _, _, _) => ??(tree)
//
//  }
//
//  def convertDeclaratorType(decl: NoptrdeclaratorContext): (Type => Type) = decl match {
//    case Noptrdeclarator0(declid, _) => t => t
//    case Noptrdeclarator1(noptrdecl, params) =>
//      t => convertDeclaratorType(noptrdecl)(CFunctionType(convertParametersandqualifiers(params), t))
//    case Noptrdeclarator2(noptrdecl, "[", maybeConstExpr, "]", _) => {
//      failIfDefined(maybeConstExpr, "Qualifiers in array dimensions are unsupported")
//      (t => convertDeclaratorType(noptrdecl)(addDims(t, 1)))
//    }
//    //case Noptrdeclarator3("(", ptrdecl, ")") => ??(ptrdecl)
//  }
//
//  def tCell(t: Type) = create.primitive_type(PrimitiveSort.Cell, t)
//
//  def tArray(t: Type) = create.primitive_type(PrimitiveSort.Array, t)
//
//  def tOpt(t: Type) = create.primitive_type(PrimitiveSort.Option, t)
//
//
//  def addDims(t: Type, dimCount: Int): Type = {
//    var result = t
//
//    if (result.isPrimitive(PrimitiveSort.Option)) {
//      result = result.firstarg.asInstanceOf[Type]
//    }
//
//    for (_ <- 0 until dimCount) {
//      result = tArray(tCell(result))
//    }
//
//    tOpt(result)
//  }
//
//
//  /**
//   * //////////////////////////////////////////
//   * ///////////////// \Types /////////////////
//   * //////////////////////////////////////////
//   */
//  /**
//   * ///////////////////////////////////////////////////////
//   * ///////////////// CLASS && Functions //////////////////
//   * ///////////////////////////////////////////////////////
//   */
//  def convertClassname(tree: ClassnameContext): ClassType = tree match {
//    case Classname0(cppid) => create.class_type(convertCppIdentifier(cppid).name)
//    case Classname1(simptempid) => convertSimpletemplateid(simptempid)
//  }
//
//  def convertClassspecifier(tree: ClassspecifierContext): ASTClass = origin(tree, tree match {
//    case Classspecifier0(head, _, maybeMembers, _) => {
//      //TODO fill class.
//      val (classkind, attrSpecSeq, classtype, isFinal, baseclass) = convertClasshead(head)
//      val newClass = create.ast_class(classtype.getName, classkind, null, null, null);
//      val members = maybeMembers.map(convertMemberspecification).getOrElse(Seq()).foreach(newClass.add)
//      newClass
//    }
//  })
//
//  def convertClasshead(tree: ClassheadContext): (ClassKind, Seq[ASTNode], ClassType, Boolean, ASTNode) = tree match {
//    case Classhead0(classkey, maybeAttrSpecSeq, headname, maybeVirtSpec, maybeBaseClause) => {
//      (convertClasskey(classkey), Seq.empty[ASTNode], convertClassheadname(headname), maybeVirtSpec.isDefined, null)
//    }
//    case Classhead1(classkey, maybeAttrSpecSeq, maybeBaseClause) => {
//      //TODO see how you can handle this. Maybe add the anonymous class as an ASTClass with a given name.
//      (convertClasskey(classkey), Seq.empty[ASTNode], null, false, null)
//    }
//  }
//
//  def convertClassheadname(tree: ClassheadnameContext) = origin(tree, tree match {
//    //TODO What to do with the nestednamespec
//    case Classheadname0(nestednamespec, classname) => convertClassname(classname)
//  })
//
//  def convertClasskey(tree: ClasskeyContext) = tree match {
//    case Classkey0("class") => ClassKind.Plain
//    //TODO Find out how C handles structs.
//    case Classkey1("struct") => ??(tree)
//    //TODO Unions are interesting. Maybe we can make a domain Either with two type parameters.
//    //      Which can then be nested to get the different values. We could also generate it.
//    case Classkey2("union") => ??(tree)
//  }
//
//  def convertMemberspecification(tree: MemberspecificationContext): Seq[ASTDeclaration] = origin(tree, tree match {
//    case Memberspecification0(memberdecl, maybeMemberspec) => convertMemberdeclaration(memberdecl) ++ maybeMemberspec.map(convertMemberspecification).getOrElse(Seq())
//
//    case Memberspecification1(accessspec, _, memberspec) => memberspec.map(convertMemberspecification).getOrElse(Seq())
//  })
//
//  def convertMemberdeclaration(tree: MemberdeclarationContext): Seq[ASTDeclaration] = origin(tree, tree match {
//    case Memberdeclaration0(attrspecseq, maybeDeclspecseq, maybeMemberdecllist, _) => {
//      val returnType = maybeDeclspecseq match {
//        case None => create primitive_type (PrimitiveSort.Void)
//        //TODO fix the line below
//        case Some(declSpecSeq) =>
//          val specs = new DeclSpecs
//          specs.add(declSpecSeq)
//          getOrFail(declSpecSeq, specs.getType)
//      }
//      val result = maybeMemberdecllist match {
//        case None => Seq()
//        case Some(memberlist) => convertMemberdeclatorlist(memberlist, returnType)
//      }
//      result
//    }
//    case Memberdeclaration1(functionDef) => Seq(convertFunctiondefinition(functionDef))
//    case Memberdeclaration6(emptyDecl) => Seq()
//    case _ => ??(tree)
//  })
//
//  def convertMemberdeclatorlist(tree: MemberdeclaratorlistContext, t: Type): Seq[DeclarationStatement] = tree match {
//    case Memberdeclaratorlist0(memberdecl) => Seq(convertMemberdeclarator(memberdecl, t))
//    case Memberdeclaratorlist1(memberdecllist, _, memberdecl) => convertMemberdeclatorlist(memberdecllist, t) :+ convertMemberdeclarator(memberdecl, t)
//  }
//
//  def convertMemberdeclarator(tree: MemberdeclaratorContext, t: Type): DeclarationStatement = tree match {
//    //TODO check if you want to know if it is final or not.
//    case Memberdeclarator0(decl, maybevirtspecseq, maybepurespec) =>
//      val actualType = convertDeclaratorType(decl)(t)
//      create.field_decl(convertDeclaratorName(decl), actualType, null)
//    case Memberdeclarator1(decl, maybebraceorequalinit) => {
//      val initValue = maybebraceorequalinit match {
//        case None => null
//        case Some(braceorequalinit) => convertBraceorequalinitializer(braceorequalinit)
//      }
//      val actualType = convertDeclaratorType(decl)(t)
//      create.field_decl(convertDeclaratorName(decl), actualType, initValue)
//    }
//    case _ => ??(tree)
//  }
//
//  def convertVirtspecifierseq(tree: VirtspecifierseqContext): Seq[ASTReserved] = tree match {
//    case Virtspecifierseq0(virtspec) => convertVirtspecifier(virtspec)
//    case Virtspecifierseq1(virtspecseq, virtspec) => {
//      convertVirtspecifierseq(virtspecseq) ++ convertVirtspecifier(virtspec)
//    }
//  }
//
//  def convertVirtspecifier(tree: VirtspecifierContext) = tree match {
//    case Virtspecifier0(overrideKey) => Seq()
//    case Virtspecifier1(finalKey) => Seq(ASTReserved.Final)
//  }
//
//  def convertFunctiondefinition(tree: FunctiondefinitionContext): ASTDeclaration = origin(tree, tree match {
//    case Functiondefinition0(maybeContract, maybeAttrSeq, maybeDeclSpecSeq, decl, maybeVirtSpecSeq, funcBody) => {
//      val name = decl match {
//        case Declarator0(Ptrdeclarator0(Noptrdeclarator1(noptrdecl, params))) => convertNoptrdeclarator(noptrdecl)
//        case Declarator1(noptr, params, trailingReturnType) => convertDeclaratorName(noptr)
//      }
//
//      val body: ASTNode = convertFunctionbody(funcBody)
//      val attrseq = maybeAttrSeq match {
//        case None => Seq()
//        case Some(attrSeq) => ???
//      }
//
//      val rawReturnType = maybeDeclSpecSeq match {
//        case None => create primitive_type (PrimitiveSort.Void)
//        case Some(declSpecSeq) => {
//          val specs = new DeclSpecs
//          specs.add(declSpecSeq)
//          convertDeclaratorType(decl)(getOrFail(declSpecSeq, specs.getType))
//        }
//      }
//
//      if (!rawReturnType.isInstanceOf[CFunctionType]) {
//        fail(decl, "This declarator specifies something that is not a function at the top level.")
//      }
//
//      val returnType = rawReturnType.asInstanceOf[CFunctionType]
//
//      val virtspecseq = maybeVirtSpecSeq match {
//        case None => Seq()
//        case Some(virtSpecSeq) => ??(virtSpecSeq)
//      }
//      val contract = getContract(convertValContract(maybeContract))
//      //TODO declspecseq(which can contain the kind etc)
//      create.method_kind(Kind.Plain, returnType.returnType, contract,
//        name, returnType.params.map(_.asDecl.get).toArray, body)
//    }
//  })
//
//
//  def convertFunctionbody(tree: FunctionbodyContext): BlockStatement = origin(tree, tree match {
//    case Functionbody0(_, Compoundstatement0("{", None, "}")) => new BlockStatement() //TODO what was the ASTNode for a body
//    case Functionbody0(_, Compoundstatement0("{", Some(stmntSeq), "}")) => {
//      val block = new BlockStatement()
//      convertStatementseq(stmntSeq).foreach(block.add)
//      block
//    }
//    case Functionbody1(functryblock) => ??(tree)
//    case Functionbody2(_, default, _) => ??(tree)
//    case Functionbody3(_, delete, _) => ??(tree)
//  })
//
//
//  def convertParametersandqualifiers(tree: ParametersandqualifiersContext) = tree match {
//    case Parametersandqualifiers0("(", parameterdeclarationclause, ")", mabyeCvqualifierseq, maybeRefqualifier, maybeExceptionspecification, maybeAttributespecifierseq) => {
//      convertParameterdeclarationclause(parameterdeclarationclause)
//    }
//  }
//
//  def convertParameterdeclarationclause(tree: ParameterdeclarationclauseContext): Seq[ParamSpec] = tree match {
//    case Parameterdeclarationclause0(Some(paramdecllist), _) => convertParameterdeclarationlist(paramdecllist)
//    case Parameterdeclarationclause0(None, _) => Seq()
//    case Parameterdeclarationclause1(_, _, _) => ??(tree)
//  }
//
//  def convertParameterdeclarationlist(tree: ParameterdeclarationlistContext): Seq[ParamSpec] = tree match {
//    case Parameterdeclarationlist0(paramdecl) => Seq(convertParameterdeclaration(paramdecl))
//    case Parameterdeclarationlist1(paramdecllist, _, paramdecl) => convertParameterdeclarationlist(paramdecllist) :+ convertParameterdeclaration(paramdecl)
//  }
//
//  def convertParameterdeclaration(tree: ParameterdeclarationContext): ParamSpec = tree match {
//    case Parameterdeclaration0(maybeAttrspecseq, declspecseq, decl) => {
//      val specs = new DeclSpecs
//      specs.add(declspecseq)
//
//      val returnType = convertDeclaratorType(decl)(getOrFail(declspecseq, specs.getType))
//      val name = convertDeclaratorName(decl)
//      ParamSpec(Some(returnType), Some(name))
//    }
//    case Parameterdeclaration1(maybeAttrspecseq, declspecseq, decl, _, initclause) => ??(tree)
//    case Parameterdeclaration2(maybeAttrspecseq, declspecseq, maybeAbstractdecl) => ??(tree)
//    case Parameterdeclaration3(maybeAttrspecseq, declspecseq, maybeAbstractdecl, _, initclause) => ??(tree)
//  }
//
//  /**
//   * ////////////////////////////////////////////////////////
//   * ///////////////// \CLASS && Functions //////////////////
//   * ////////////////////////////////////////////////////////
//   */
//  /**
//   * //////////////////////////////////////////////////////////////
//   * ///////////////// Expressions && Statements //////////////////
//   * //////////////////////////////////////////////////////////////
//   */
//  def expr(exp: LangExprContext): ASTNode = exp match {
//    case LangExpr0(e) => expr(e)
//  }
//
//
//  def expr(tree: ParserRuleContext): ASTNode = origin(tree, tree match {
//    case Expression0(assignmentExpr) => expr(assignmentExpr)
//
//    case Constantexpression0(condexpr) => expr(condexpr)
//
//    case Assignmentexpression0(conditionalexpr) => expr(conditionalexpr)
//    case Assignmentexpression1(logicalor, assignop, initclause) => {
//      assignop match {
//        case Assignmentoperator0("=") =>
//        //TODO check if the other assignmentoperators are available
//        case _ => ??(tree)
//      }
//      create assignment(
//        expr(logicalor),
//        convertInitializerclause(initclause)
//      )
//    }
//    case Assignmentexpression2(throwexpr) => ??(tree)
//
//    case Conditionalexpression0(logicalExpr) => expr(logicalExpr)
//    case Conditionalexpression1(cond, _, yes, _, no) => create expression(StandardOperator.ITE, expr(cond), expr(yes), expr(no))
//    case Conditionalexpression2(lft, "==>", rght) => create expression(StandardOperator.Implies, expr(lft), expr(rght))
//
//
//    case Logicalorexpression0(andexpr) => expr(andexpr)
//    case Logicalorexpression1(lft, _, rght) => create expression(StandardOperator.Or, expr(lft), expr(rght))
//
//    case Logicalandexpression0(inclusiveorexpr) => expr(inclusiveorexpr)
//    case Logicalandexpression1(lft, _, rght) => create expression(StandardOperator.And, expr(lft), expr(rght))
//    case Logicalandexpression2(lft, _, rght) => create expression(StandardOperator.Star, expr(lft), expr(rght))
//
//    case Inclusiveorexpression0(exclexpr) => expr(exclexpr)
//    case Inclusiveorexpression1(lft, "|", rght) => create expression(StandardOperator.BitOr, expr(lft), expr(rght))
//
//    case Exclusiveorexpression0(andexpr) => expr(andexpr)
//    case Exclusiveorexpression1(lft, "^", rght) => create expression(StandardOperator.BitXor, expr(lft), expr(rght))
//
//    case Andexpression0(eqexpr) => expr(eqexpr)
//    case Andexpression1(lft, "&", rght) => create expression(StandardOperator.BitAnd, expr(lft), expr(rght))
//
//    case Equalityexpression0(relationalexpr) => expr(relationalexpr)
//    case Equalityexpression1(lft, "==", rght) => create expression(StandardOperator.EQ, expr(lft), expr(rght))
//    case Equalityexpression2(lft, "!=", rght) => create expression(StandardOperator.NEQ, expr(lft), expr(rght))
//
//    case Relationalexpression0(shiftexpr) => expr(shiftexpr)
//    case Relationalexpression1(lft, "<", rght) => create expression(StandardOperator.LT, expr(lft), expr(rght))
//    case Relationalexpression2(lft, ">", rght) => create expression(StandardOperator.GT, expr(lft), expr(rght))
//    case Relationalexpression3(lft, "<=", rght) => create expression(StandardOperator.LTE, expr(lft), expr(rght))
//    case Relationalexpression4(lft, ">=", rght) => create expression(StandardOperator.GTE, expr(lft), expr(rght))
//
//    case Shiftexpression0(additiveexpr) => expr(additiveexpr)
//    case Shiftexpression1(lft, shiftop, rght) => {
//      create expression(
//        shiftop match {
//          case Shiftoperator0(">>") => StandardOperator.RightShift
//          case Shiftoperator1("<<") => StandardOperator.LeftShift
//        },
//        expr(lft),
//        expr(rght)
//      )
//    }
//
//    case Additiveexpression0(mulexpr) => expr(mulexpr)
//    case Additiveexpression1(lft, "+", rght) => create expression(StandardOperator.Plus, expr(lft), expr(rght))
//    case Additiveexpression2(lft, "-", rght) => create expression(StandardOperator.Minus, expr(lft), expr(rght))
//
//    case Multiplicativeexpression0(pmexpr) => expr(pmexpr)
//    case Multiplicativeexpression1(lft, mulop, rght) => {
//      val op = mulop match {
//        case MultiplicativeOp0("*") => StandardOperator.Mult
//        case MultiplicativeOp1("/") => StandardOperator.FloorDiv
//        case MultiplicativeOp2("%") => StandardOperator.Mod
//        case MultiplicativeOp3(valMulop) => StandardOperator.Div
//      }
//      create expression(op, expr(lft), expr(rght))
//    }
//
//    case Pmexpression0(castexpr) => expr(castexpr)
//    case Pmexpression1(lft, ".*", rght) => ??(tree)
//    case Pmexpression2(lft, "->*", rght) => ??(tree)
//
//    case Castexpression0(unaryexpr) => expr(unaryexpr)
//    case Castexpression1("(", thetypeid, ")", castexpr) => ??(tree)
//
//    case Unaryexpression0(postfixexpr) => expr(postfixexpr)
//    case Unaryexpression1("++", castexpression) => ??(tree)
//    case Unaryexpression2("--", castexpression) => ??(tree)
//    case Unaryexpression3(unaryop, castexpr) => {
//      val op = unaryop match {
//        case Unaryoperator0("|") => ??(unaryop)
//        case Unaryoperator1("*") => StandardOperator.Indirection
//        case Unaryoperator2("&") => StandardOperator.AddrOf
//        case Unaryoperator3("+") => ??(unaryop)
//        case Unaryoperator4(notSymbol) => StandardOperator.Not
//        case Unaryoperator5("~") => StandardOperator.BitNot
//        case Unaryoperator6("-") => StandardOperator.UMinus
//        case Unaryoperator7(notWord) => StandardOperator.Not
//      }
//      create expression(op, expr(castexpr))
//    }
//    case Unaryexpression4(sizeof, unaryexpression) => ??(tree)
//    case Unaryexpression5(sizeof, "(", thetypeid, ")") => ??(tree)
//    case Unaryexpression6(sizeof, "...", "(", cppIdentifier, ")") => ??(tree)
//    case Unaryexpression7(alignof, "(", thetypeid, ")") => ??(tree)
//    case Unaryexpression8(noexceptexpression) => ??(tree)
//    //TODO I think we need to support this.
//    case Unaryexpression9(newexpression) => ??(tree)
//    //TODO Look at the usage of the destructors in SYCL
//    case Unaryexpression10(deleteexpression) => ??(tree)
//
//    //Other cases ommited
//
//    case Postfixexpression0(primaryexpr) => expr(primaryexpr)
//    case Postfixexpression1(postfix, "[", expression, "]") => create expression(Subscript, expr(postfix), expr(expression))
//    case Postfixexpression3(postexpr, "(", maybeArgs, ")") => {
//      val args = maybeArgs.map(convertExpressionlist).getOrElse(Seq())
//      val methodExpr = expr(postexpr)
//
//      if (!methodExpr.isInstanceOf[NameExpression]) {
//        ??(postexpr)
//      }
//
//      val methodName = methodExpr.asInstanceOf[NameExpression].getName
//      create invokation(null, null, methodName, args: _*)
//    }
//    case Postfixexpression8(postfixexpr, ".", maybeTemplate, idexpr) =>
//      create dereference(expr(postfixexpr), convertIdexpression(idexpr).name)
//
//
//    case Postfixexpression12(postexpr, "++") => create expression(PostIncr, expr(postexpr))
//    case Postfixexpression13(postexpr, "--") => create expression(PostDecr, expr(postexpr))
//    //Other cases ommited
//
//    //    case Postfixexpression2(postfixexpr, "[", bracedinitlist, "]") => ??(tree)
//    //    case Postfixexpression4(simpletypespec, "(", maybeExprlist, ")") => ??(tree)
//    //    case Postfixexpression5(typenamespec, "(", maybeExprlist, ")") => ??(tree)
//    //    case Postfixexpression6(simpletypespec, bracedinitlist,) => ??(tree)
//    //    case Postfixexpression7(typenamespec, bracedinitlist,) => ??(tree)
//    //    case Postfixexpression9(postfixexpr, "->", maybeTemplate, idexpr) => ??(tree)
//    //    case Postfixexpression10(postfixexpr, ".", pseudodestructorname) => ??(tree)
//    //    case Postfixexpression11(postfixexpr, "->", pseudodestructorname) => ??(tree)
//    //    case Postfixexpression14(dynamic_cast, "<", thetypeid, ">", "(", expression, ")") => ??(tree)
//    //    case Postfixexpression15(static_cast, "<", thetypeid, ">", "(", expression, ")") => ??(tree)
//    //    case Postfixexpression16(reinterpret_cast, "<", thetypeid, ">", "(", expression, ")") => ??(tree)
//    //    case Postfixexpression17(const_cast, "<", thetypeid, ">", "(", expression, ")") =>  ??(tree)
//    //    case Postfixexpression18(typeidofthetypeid, "(", expression, ")") =>  ??(tree)
//    //    case Postfixexpression19(typeidofthetypeid, "(", thetypeid, ")") => ??(tree)
//
//    case Primaryexpression0(literalexpr) => expr(literalexpr)
//    case Primaryexpression1(_) => create reserved_name ASTReserved.This
//    case Primaryexpression2("(", bracketExpr, ")") => expr(bracketExpr)
//    case Primaryexpression3(idexpr) => convertIdexpression(idexpr)
//    case Primaryexpression4(lambda) => ??(tree)
//    case Primaryexpression5(valPrimary) => valExpr(valPrimary)
//
//    case Literal0(integer) => create constant Integer.parseInt(integer)
//    case Literal1(characterlit) => ??(tree)
//    case Literal2(floatlit) => ??(tree)
//    case Literal3(stringlit) => ??(tree)
//    case Literal4(bool) => bool match {
//      case Booleanliteral0("false") => create constant (false)
//      case Booleanliteral1("true") => create constant (true)
//    }
//    case Literal5(pointer) => create reserved_name ASTReserved.Null
//    case Literal6(userdeflit) => ??(tree)
//
//
//    case Condition0(expression) => expr(expression)
//    case Condition1(maybeAttributespecifierseq, declspecifierseq, declarator, "=", initializerclause) => ??(tree)
//    case Condition2(maybeAttributespecifierseq, declspecifierseq, declarator, bracedinitlist) => ??(tree)
//
//    case _ => ??(tree)
//  })
//
//
//  def convertStatementseq(tree: StatementseqContext): Seq[ASTNode] = origin(tree, tree match {
//    case Statementseq0(stmnt) => Seq(convertStatement(stmnt))
//    case Statementseq1(stmntseq, stmnt) => convertStatementseq(stmntseq) :+ convertStatement(stmnt)
//  })
//
//  def convertStatement(tree: ParserRuleContext): ASTNode = origin(tree, tree match {
//    case Statement0(labeledStmnt) => convertStatement(labeledStmnt)
//    case Statement1(declStmnt) => convertDeclarationstatement(declStmnt)
//    case Statement2(_, exprStmnt) => convertStatement(exprStmnt)
//    case Statement3(_, compoundStmnt) => convertStatement(compoundStmnt)
//    case Statement4(_, selectionStmnt) => convertStatement(selectionStmnt)
//    case Statement5(_, iterationStmnt) => convertStatement(iterationStmnt)
//    case Statement6(_, jumpStmnt) => convertStatement(jumpStmnt)
//    case Statement7(_, tryblock) => convertStatement(tryblock)
//    case Statement8(valstmnt) => create block (convertValStat(valstmnt): _*)
//    case Statement9(valstmnt) => convertValStat(valstmnt)
//
//    case Labeledstatement0(_) => ??(tree)
//    case Labeledstatement1(_) => ??(tree)
//    case Labeledstatement2(_) => ??(tree)
//
//    case Expressionstatement0(maybeExprstmnt, _) => maybeExprstmnt.map(expr).getOrElse(new BlockStatement)
//
//    case Compoundstatement0(_, maybeStmntSeq, _) => create.block(maybeStmntSeq.map(convertStatementseq).getOrElse(Seq()): _*)
//
//    case Selectionstatement0("if", "(", condition, ")", ifstmnt) => {
//      create ifthenelse(expr(condition), convertStatement(ifstmnt), null)
//    }
//    case Selectionstatement1("if", "(", condition, ")", ifstmnt, "else", elsestmnt) => {
//      create ifthenelse(expr(condition), convertStatement(ifstmnt), convertStatement(elsestmnt))
//    }
//    case Selectionstatement2("switch", "(", condition, ")", switchstmnt) => ??(tree)
//
//    case Iterationstatement0(maybeContract1, "while", "(", condition, ")", maybeContract2, stmnt) => {
//      create while_loop(expr(condition), convertStatement(stmnt), getContract(convertValContract(maybeContract1), convertValContract(maybeContract2)))
//    }
//    case Iterationstatement1(maybeContr1, "do", stmnt, "while", "(", expr, ")", _) => ??(tree)
//    case Iterationstatement2(maybeContr1, "for", "(", initStat, maybeCond, ";", maybeUpdate, ")", maybeContr2, body) => {
//      val contract = getContract(convertValContract(maybeContr1), convertValContract(maybeContr2))
//
//      val loop = create for_loop(
//        convertStatement(initStat),
//        maybeCond.map(expr).orNull,
//        maybeUpdate.map(expr).orNull,
//        convertStatement(body)
//      )
//      loop.setContract(contract)
//      loop
//    }
//    case Iterationstatement3(maybeContr1, "for", "(", _, ":", _, ")", maybeContr2, stmnt) => ??(tree)
//
//    case Jumpstatement0("break", ";") => create special ASTSpecial.Kind.Break
//    case Jumpstatement1("continue", ";") => create special ASTSpecial.Kind.Continue
//    case Jumpstatement2("return", None, ";") => create return_statement()
//    case Jumpstatement2("return", Some(returnexpr), ";") => create return_statement (expr(returnexpr))
//    case Jumpstatement3("return", binitList, ";") => create return_statement (convertBracedinitlist(binitList): _*)
//    case Jumpstatement4("goto", cppId, ";") => create special(ASTSpecial.Kind.Goto, create label (convertCppIdentifier(cppId).name))
//
//    case Forinitstatement0(exprStat) => convertStatement(exprStat)
//    case Forinitstatement1(simpleDecl) => convertStatement(simpleDecl)
//
//    case Simpledeclaration0(maybeContract, declSpecSeq, Some(initDeclList), _) => {
//      // TODO find out what the contract here is
//      val specs = new DeclSpecs
//      specs.add(declSpecSeq)
//      val typeofVar = getOrFail(declSpecSeq, specs.getType)
//      val result = new VariableDeclaration(typeofVar)
//      convertInitdeclaratorlist(initDeclList).foreach {
//        case (name, init) =>
//          result.add(DeclarationStatement(name, VariableDeclaration.common_type, init))
//      }
//      result
//    }
// /*   case Simpledeclaration0(maybeContract, Some(declSpecSeq), Some(initDeclList), _) => {
//      // TODO find out what the contract here is
//      val specs = new DeclSpecs
//      specs.add(declSpecSeq)
//      val typeofVar = getOrFail(declSpecSeq, specs.getType)
//      val result = new VariableDeclaration(typeofVar)
//      convertInitdeclaratorlist(initDeclList).foreach {
//        case (name, init) =>
//          result.add(DeclarationStatement(name, VariableDeclaration.common_type, init))
//      }
//      result
//    }*/
//    case Simpledeclaration0(maybeContract, declSpec, _, _) => {
//          val specs = new DeclSpecs
//          specs.add(declSpec)
//          if (specs.decls.isEmpty) {
//            fail(tree, "Expected an ASTDeclaration.")
//          }
//          specs.decls.head
//      }
//      /*maybeDeclSpec match {
//        case None => new BlockStatement
//        case Some(declSpec) => {
//          val specs = new DeclSpecs
//          specs.add(declSpec)
//          if (specs.decls.isEmpty) {
//            fail(tree, "Expected an ASTDeclaration.")
//          }
//          specs.decls.head
//        }
//      }
//    }*/
//    case Simpledeclaration1(maybeContract, attrSpecSeq, maybeDecl, _, _) => ??(tree)
//  })
//
//  /**
//   * //////////////////////////////////////////////////////////////
//   * ///////////////// \Expressions && Statements //////////////////
//   * //////////////////////////////////////////////////////////////
//   */
//
//  /**
//   * //////////////////////////////////////////////////
//   * ///////////////// Val functions //////////////////
//   * //////////////////////////////////////////////////
//   */
//  def convertValExpList(args: ValExpressionListContext): Seq[ASTNode] = args match {
//    case ValExpressionList0(exp) =>
//      Seq(expr(exp))
//    case ValExpressionList1(exp, ",", expList) =>
//      expr(exp) +: convertValExpList(expList)
//  }
//
//  def convertValClause(clause: ValContractClauseContext) = (builder: ContractBuilder) => clause match {
//    case ValContractClause0(_modifies, names, _) =>
//      builder.modifies(convertValExpList(names): _*)
//    case ValContractClause1(_accessible, names, _) =>
//      builder.accesses(convertValExpList(names): _*)
//    case ValContractClause2(_requires, exp, _) =>
//      builder.requires(expr(exp))
//    case ValContractClause3(_ensures, exp, _) =>
//      builder.ensures(expr(exp))
//    case ValContractClause4(_given, t, name, _) =>
//      builder.`given`(create.field_decl(convertLangId(name), convertTypespecifier(t)))
//    case ValContractClause5(_yields, t, name, _) =>
//      builder.yields(create.field_decl(convertLangId(name), convertTypespecifier(t)))
//    case ValContractClause6(_context_everywhere, exp, _) =>
//      builder.appendInvariant(expr(exp))
//    case ValContractClause7(_context, exp, _) =>
//      builder.context(expr(exp))
//    case ValContractClause8(_loop_invariant, exp, _) =>
//      builder.appendInvariant(expr(exp))
//  }
//
//  def convertValBlock(block: ValBlockContext): BlockStatement = origin(block, block match {
//    case ValBlock0("{", statements, "}") =>
//      create block (statements.map(convertValStat): _*)
//  })
//
//  def convertValStat(stat: ValEmbedStatementBlockContext): Seq[ASTNode] = origin(stat, stat match {
//    case ValEmbedStatementBlock0(_startSpec, stats, _endSpec) =>
//      stats.map(convertValStat)
//    case ValEmbedStatementBlock1(stats) =>
//      stats.map(convertValStat)
//  })
//
//  def convertValStat(stat: LangStatementContext): Seq[ASTNode] = origin(stat, stat match {
//    case LangStatement0(stat) =>
//      Seq(convertStatement(stat))
//  })
//
//  def convertValStat(stat: ValStatementContext): ASTNode = origin(stat, stat match {
//    case ValStatement0(_create, block) =>
//      create lemma (convertValBlock(block))
//    case ValStatement1(_qed, exp, _) =>
//      create special(ASTSpecial.Kind.QED, expr(exp))
//    case ValStatement2(_apply, exp, _) =>
//      create special(ASTSpecial.Kind.Apply, expr(exp))
//    case ValStatement3(_use, exp, _) =>
//      create special(ASTSpecial.Kind.Use, expr(exp))
//    case ValStatement4(_create, hist, _) =>
//      create special(ASTSpecial.Kind.CreateHistory, expr(hist))
//    case ValStatement5(_create, fut, _, proc, _) =>
//      create special(ASTSpecial.Kind.CreateFuture, expr(fut), expr(proc))
//    case ValStatement6(_destroy, hist, _, proc, _) =>
//      create special(ASTSpecial.Kind.DestroyHistory, expr(hist), expr(proc))
//    case ValStatement7(_destroy, fut, _) =>
//      create special(ASTSpecial.Kind.DestroyFuture, expr(fut))
//    case ValStatement8(_split, fut, _, perm1, _, proc1, _, perm2, _, proc2, _) =>
//      create special(ASTSpecial.Kind.SplitHistory, expr(fut), expr(perm1), expr(proc1), expr(perm2), expr(proc2))
//    case ValStatement9(_merge, fut, _, perm1, _, proc1, _, perm2, _, proc2, _) =>
//      create special(ASTSpecial.Kind.MergeHistory, expr(fut), expr(perm1), expr(proc1), expr(perm2), expr(proc2))
//    case ValStatement10(_choose, fut, _, perm, _, proc1, _, proc2, _) =>
//      create special(ASTSpecial.Kind.ChooseHistory, expr(fut), expr(perm), expr(proc1), expr(proc2))
//    case ValStatement11(_fold, pred, _) =>
//      create special(ASTSpecial.Kind.Fold, expr(pred))
//    case ValStatement12(_unfold, pred, _) =>
//      create special(ASTSpecial.Kind.Unfold, expr(pred))
//    case ValStatement13(_open, pred, _) =>
//      create special(ASTSpecial.Kind.Open, expr(pred))
//    case ValStatement14(_close, pred, _) =>
//      create special(ASTSpecial.Kind.Close, expr(pred))
//    case ValStatement15(_assert, assn, _) =>
//      create special(ASTSpecial.Kind.Assert, expr(assn))
//    case ValStatement16(_assume, assn, _) =>
//      create special(ASTSpecial.Kind.Assume, expr(assn))
//    case ValStatement17(_inhale, res, _) =>
//      create special(ASTSpecial.Kind.Inhale, expr(res))
//    case ValStatement18(_exhale, res, _) =>
//      create special(ASTSpecial.Kind.Exhale, expr(res))
//    case ValStatement19(_label, lbl, _) =>
//      create special(ASTSpecial.Kind.Label, convertIDName(lbl))
//    case ValStatement20(_refute, assn, _) =>
//      create special(ASTSpecial.Kind.Refute, expr(assn))
//    case ValStatement21(_witness, pred, _) =>
//      create special(ASTSpecial.Kind.Witness, expr(pred))
//    case ValStatement22(_ghost, code) =>
//      flattenIfSingleStatement(convertValStat(code))
//    case ValStatement23(_send, res, _to, lbl, _, thing, _) =>
//      create special(ASTSpecial.Kind.Send, expr(res), convertIDName(lbl), expr(thing))
//    case ValStatement24(_recv, res, _from, lbl, _, thing, _) =>
//      create special(ASTSpecial.Kind.Recv, expr(res), convertIDName(lbl), expr(thing))
//    case ValStatement25(_transfer, exp, _) =>
//      ??(stat)
//    case ValStatement26(_csl_subject, obj, _) =>
//      create special(ASTSpecial.Kind.CSLSubject, expr(obj))
//    case ValStatement27(_spec_ignore, "}") =>
//      create special ASTSpecial.Kind.SpecIgnoreEnd
//    case ValStatement28(_spec_ignore, "{") =>
//      create special ASTSpecial.Kind.SpecIgnoreStart
//    case ValStatement29(_action, arg1, _, arg2, _, arg3, _, arg4, map, _) =>
//      if (map.nonEmpty) {
//        ??(map.head)
//      }
//      create special(ASTSpecial.Kind.ActionHeader, expr(arg1), expr(arg2), expr(arg3), expr(arg4))
//    case ValStatement30(_atomic, _, resList, _, stat) =>
//      create csl_atomic(create block (convertValStat(stat): _*), resList.map(convertValExpList).getOrElse(Seq()).map {
//        case name: NameExpression if name.getKind == NameExpressionKind.Unresolved =>
//          create label name.getName
//        case other => other
//      }: _*)
//  })
//
//  def valExpr(exp: ValPrimaryContext): ASTNode = origin(exp, exp match {
//    case ValPrimary0(t, "{", maybeExps, "}") =>
//      val exps = maybeExps.map(convertValExpList).getOrElse(Seq())
//      create struct_value(convertTypespecifier(t), null, exps: _*)
//    case ValPrimary1("[", factor, "]", exp) =>
//      create expression(Scale, expr(factor), expr(exp))
//    case ValPrimary2("|", seq, "|") =>
//      create expression(Size, expr(seq))
//    case ValPrimary3("\\unfolding", pred, "\\in", exp) =>
//      create expression(Unfolding, expr(pred), expr(exp))
//    case ValPrimary4("(", exp, "!", indepOf, ")") =>
//      create expression(IndependentOf, expr(exp), convertIDName(indepOf))
//    case ValPrimary5("(", x, "\\memberof", xs, ")") =>
//      create expression(Member, expr(x), expr(xs))
//    case ValPrimary6("{", from, "..", to, "}") =>
//      create expression(RangeSeq, expr(from), expr(to))
//    case ValPrimary7("*") =>
//      create reserved_name ASTReserved.Any
//    case ValPrimary8("\\current_thread") =>
//      create reserved_name ASTReserved.CurrentThread
//    case ValPrimary9(_, binderName, t, id, "=", fr, "..", to, _, main, _) =>
//      val name = convertLangId(id)
//      val decl = create field_decl(name, convertTypespecifier(t))
//      val guard = create expression(StandardOperator.And,
//        create expression(LTE, expr(fr), create unresolved_name (name)),
//        create expression(StandardOperator.LT, create unresolved_name (name), expr(to))
//      )
//      binderName match {
//        case "\\forall*" => create starall(guard, expr(main), decl)
//        case "\\forall" => create forall(guard, expr(main), decl)
//        case "\\exists" => create exists(guard, expr(main), decl)
//      }
//    case ValPrimary10(_, binderName, t, id, _, guard, _, main, _) =>
//      val decl = create field_decl(convertLangId(id), convertTypespecifier(t))
//      binderName match {
//        case "\\forall*" => create starall(expr(guard), expr(main), decl)
//        case "\\forall" => create forall(expr(guard), expr(main), decl)
//        case "\\exists" => create exists(expr(guard), expr(main), decl)
//      }
//    case ValPrimary11(_, "\\let", t, id, "=", exp, _, body, _) =>
//      create let_expr(create field_decl(convertLangId(id), convertTypespecifier(t), expr(exp)), expr(body))
//    case ValPrimary12(_, "\\sum", t, id, _, guard, _, main, _) =>
//      create summation(expr(guard), expr(main), create field_decl(convertLangId(id), convertTypespecifier(t)))
//    case ValPrimary13("\\length", "(", exp, ")") =>
//      create expression(Length, expr(exp))
//    case ValPrimary14("\\old", "(", exp, ")") =>
//      create expression(Old, expr(exp))
//    case ValPrimary15("\\id", "(", exp, ")") =>
//      create expression(Identity, expr(exp))
//    case ValPrimary16("\\typeof", "(", exp, ")") =>
//      create expression(TypeOf, expr(exp))
//    case ValPrimary17("\\matrix", "(", m, _, size0, _, size1, ")") =>
//      create expression(ValidMatrix, expr(m), expr(size0), expr(size1))
//    case ValPrimary18("\\array", "(", a, _, size0, ")") =>
//      create expression(ValidArray, expr(a), expr(size0))
//    case ValPrimary19("\\pointer", "(", p, _, size0, _, perm, ")") =>
//      create expression(ValidPointer, expr(p), expr(size0), expr(perm))
//    case ValPrimary20("\\pointer_index", "(", p, _, idx, _, perm, ")") =>
//      create expression(ValidPointerIndex, expr(p), expr(idx), expr(perm))
//    case ValPrimary21("\\values", "(", a, _, fr, _, to, ")") =>
//      create expression(Values, expr(a), expr(fr), expr(to))
//    case ValPrimary22("\\sum", "(", a, _, b, ")") =>
//      create expression(FoldPlus, expr(a), expr(b))
//    case ValPrimary23("\\vcmp", "(", a, _, b, ")") =>
//      create expression(VectorCompare, expr(a), expr(b))
//    case ValPrimary24("\\vrep", "(", v, ")") =>
//      create expression(VectorRepeat, expr(v))
//    case ValPrimary25("\\msum", "(", a, _, b, ")") =>
//      create expression(MatrixSum, expr(a), expr(b))
//    case ValPrimary26("\\mcmp", "(", a, _, b, ")") =>
//      create expression(MatrixCompare, expr(a), expr(b))
//    case ValPrimary27("\\mrep", "(", m, ")") =>
//      create expression(MatrixRepeat, expr(m))
//    case ValPrimary28("Reducible", "(", exp, _, opNode, ")") =>
//      val opText = opNode match {
//        case ValReducibleOperator0("+") => "+"
//        case ValReducibleOperator1(id) => convertLangId(id)
//      }
//      create expression(opText match {
//        case "+" => ReducibleSum
//        case "min" => ReducibleMin
//        case "max" => ReducibleMax
//      }, expr(exp))
//    case ValPrimary29(label, _, exp) =>
//      val res = expr(exp)
//      res.addLabel(create label (convertLangId(label)))
//      res
//  })
//
//  def convertValOp(op: ValImpOpContext): StandardOperator = op match {
//    case ValImpOp0("-*") => StandardOperator.Wand
//    case ValImpOp1("==>") => StandardOperator.Implies
//  }
//
//  def convertValOp(op: ValAndOpContext): StandardOperator = op match {
//    case ValAndOp0("**") => StandardOperator.Star
//  }
//
//  def convertValOp(op: ValMulOpContext): StandardOperator = op match {
//    case ValMulOp0("\\") => StandardOperator.Div
//  }
//
//  def convertValReserved(reserved: ValReservedContext): NameExpression = origin(reserved, reserved match {
//    case ValReserved0(_) =>
//      fail(reserved, "This identifier is reserved and cannot be declared or used.")
//    case ValReserved1("\\result") =>
//      create reserved_name ASTReserved.Result
//    case ValReserved2("\\current_thread") =>
//      create reserved_name ASTReserved.CurrentThread
//    case ValReserved3("none") =>
//      create reserved_name ASTReserved.NoPerm
//    case ValReserved4("write") =>
//      create reserved_name ASTReserved.FullPerm
//    case ValReserved5("read") =>
//      create reserved_name ASTReserved.ReadPerm
//    case ValReserved6("None") =>
//      create reserved_name ASTReserved.OptionNone
//    case ValReserved7("empty") =>
//      create reserved_name ASTReserved.EmptyProcess
//  })
//
//  def convertOverlappingValReservedID(reserved: ValReservedContext): String = reserved match {
//    case ValReserved0(s) => s
//    case ValReserved1("\\result") => fail(reserved, "This identifier is invalid in the current language")
//    case ValReserved2("\\current_thread") => fail(reserved, "This identifier is invalid in the current language")
//    case ValReserved3(s) => s
//    case ValReserved4(s) => s
//    case ValReserved5(s) => s
//    case ValReserved6(s) => s
//    case ValReserved7(s) => s
//  }
//
//  def convertOverlappingValReservedName(reserved: ValReservedContext): NameExpression =
//    create unresolved_name convertOverlappingValReservedID(reserved)
//
//  def convertValContract(contract: Option[ValEmbedContractContext]) = (builder: ContractBuilder) => contract match {
//    case Some(ValEmbedContract0(blocks)) =>
//      for (block <- blocks) {
//        convertValContractBlock(block)(builder)
//      }
//    case None =>
//    // nop
//  }
//
//  def convertValContractBlock(contract: ValEmbedContractBlockContext) = (builder: ContractBuilder) => contract match {
//    case ValEmbedContractBlock0(_startSpec, clauses, _endSpec) =>
//      for (clause <- clauses) {
//        convertValClause(clause)(builder)
//      }
//    case ValEmbedContractBlock1(clauses) =>
//      for (clause <- clauses) {
//        convertValClause(clause)(builder)
//      }
//  }
//
//  def convertValType(t: ValTypeContext): Type = origin(t, t match {
//    case ValType0(s) => s match {
//      case "resource" => create primitive_type (PrimitiveSort.Resource)
//      case "process" => create primitive_type (PrimitiveSort.Process)
//      case "frac" => create primitive_type PrimitiveSort.Fraction
//      case "zfrac" => create primitive_type PrimitiveSort.ZFraction
//      case "rational" => create primitive_type PrimitiveSort.Rational
//      case "bool" => create primitive_type PrimitiveSort.Boolean
//    }
//    case ValType1("seq", _, subType, _) =>
//      create primitive_type(PrimitiveSort.Sequence, convertTypespecifier(subType))
//  })
//
//  def convertValArg(arg: ValArgContext): DeclarationStatement = origin(arg, arg match {
//    case ValArg0(t, id) =>
//      create field_decl(convertLangId(id), convertTypespecifier(t))
//  })
//
//  def convertValArgList(argList: ValArgListContext): Seq[DeclarationStatement] = origin(argList, argList match {
//    case ValArgList0(arg) => Seq(convertValArg(arg))
//    case ValArgList1(arg, _, args) => convertValArg(arg) +: convertValArgList(args)
//  })
//
//  def convertValModifier(modifier: ValModifierContext): NameExpression = origin(modifier, modifier match {
//    case ValModifier0(s) => s match {
//      case "pure" => create reserved_name (ASTReserved.Pure)
//      case "inline" => create reserved_name (ASTReserved.Inline)
//    }
//    case ValModifier1(langMod) => convertModifier(langMod)
//  })
//
//  def convertValModifiers(modifiers: ValEmbedModifiersContext): Seq[NameExpression] = origin(modifiers, modifiers match {
//    case ValEmbedModifiers0(_, mods, _) =>
//      mods.map(convertValModifier)
//    case ValEmbedModifiers1(mods) =>
//      mods.map(convertValModifier)
//  })
//
//  def convertValDecl(decl: ValDeclarationContext): ASTNode = origin(decl, decl match {
//    case ValDeclaration0(clauses, mods, t, name, _, args, _, body) =>
//      val contract = getContract(clauses.map(convertValClause): _*)
//      val func = create function_decl(
//        convertTypespecifier(t),
//        contract,
//        convertLangId(name),
//        args.map(convertValArgList).getOrElse(Seq()).toArray,
//        body match {
//          case ValPredicateDef0(_) => null
//          case ValPredicateDef1("=", exp, _) => expr(exp)
//        }
//      )
//      mods.foreach(mod => func.attach(convertValModifier(mod)))
//      func
//    case ValDeclaration1("axiom", name, _, left, "==", right, _) =>
//      create axiom(convertLangId(name), create expression(EQ, expr(left), expr(right)))
//    case ValDeclaration2(clauses, "ghost", langDecl) =>
//      val decl = convertDeclaration(langDecl)
//      if (clauses.nonEmpty) {
//        decl match {
//          case method: Method =>
//            method.setContract(getContract(clauses.map(convertValClause): _*))
//            method
//          case _ =>
//            fail(langDecl, "This constructor cannot have contract declarations")
//        }
//      } else {
//        decl
//      }
//  })
//
//  def convertValDecl(decl: ValEmbedDeclarationBlockContext): Seq[ASTNode] = decl match {
//    case ValEmbedDeclarationBlock0(_, decls, _) =>
//      decls.map((decl) => convertValDecl(decl))
//  }
//
//
//  def convertValWithThen(withThen: ValWithThenContext): ASTNode = withThen match {
//    case ValWithThen0("with", stat) =>
//      create special(ASTSpecial.Kind.With, flattenIfSingleStatement(convertValStat(stat)))
//    case ValWithThen1("then", stat) =>
//      create special(ASTSpecial.Kind.Then, flattenIfSingleStatement(convertValStat(stat)))
//  }
//
//  def convertValWithThen(withThen: ValEmbedWithThenBlockContext): Seq[ASTNode] = withThen match {
//    case ValEmbedWithThenBlock0(_, mappings, _) => mappings.map(convertValWithThen)
//    case ValEmbedWithThenBlock1(mappings) => mappings.map(convertValWithThen)
//  }
//
//  def convertValWithThen(withThen: ValEmbedWithThenContext): Seq[ASTNode] = withThen match {
//    case ValEmbedWithThen0(blocks) => blocks.flatMap(convertValWithThen)
//  }
//
//  /**
//   * //////////////////////////////////////////////////
//   * ///////////////// \Val functions /////////////////
//   * //////////////////////////////////////////////////
//   */
//
//  /**
//   * //////////////////////////////////////////////
//   * ///////////////// Templates //////////////////
//   * //////////////////////////////////////////////
//   */
//
//
//  /**
//   * ///////////////////////////////////////////////
//   * ///////////////// \Templates //////////////////
//   * ///////////////////////////////////////////////
//   */
//}
