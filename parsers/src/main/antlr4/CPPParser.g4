parser grammar CPPParser;
options {tokenVocab = LangCPPLexer; }
import LangCPPParser, SpecParser;

@parser::members {
    public int specLevel = 0;
}

langExpr: expression;
langId: cppIdentifier;
langType: typespecifier;
langModifier: storageclassspecifier;
langStatement: statement;
langDecl: functiondefinition;

startSpec: LineStartSpec {specLevel++;} | BlockStartSpec {specLevel++;} | BlockStartSpecImmediate {specLevel++;};
endSpec: EndSpec {specLevel--;};
