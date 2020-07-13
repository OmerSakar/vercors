package vct.parsers

import org.antlr.v4.runtime.CommonTokenStream
import vct.antlr4.generated.CPPParser
import vct.antlr4.generated.CParser.TranslationUnitContext
import vct.col.ast.stmt.decl.ProgramUnit


object CPPtoCOL {
  def convert(tree: TranslationUnitContext, fileName: String, tokens: CommonTokenStream, parser: CPPParser): ProgramUnit = {
    CPPtoCOL(fileName, tokens, parser).convertTranslationunit(tree)
  }
}

case class CPPtoCOL(fileName: String, tokens: CommonTokenStream, parser: CPPParser)
  extends ToCOL(fileName, tokens, parser) {

  def convertTranslationunit(tree: TranslationUnitContext): ProgramUnit = {

  }

}