parser grammar JavaParser;
options {tokenVocab = LangJavaLexer;}
import LangJavaParser, SpecParser;

@parser::members {
    public int specLevel = 0;
}

langExpr: expression;
langId: javaIdentifier;
langType: type;
langModifier: classOrInterfaceModifier;
langStatement: blockStatement;
langDecl: modifier* memberDeclaration;

startSpec
    : LineStartSpec {specLevel++;}
    | BlockStartSpec {specLevel++;}
    | BlockStartSpecImmediate {specLevel++;}
    ;

endSpec
    : EndSpec {specLevel--;}
    | LineCommentStartInSpec {specLevel--;}
    ;