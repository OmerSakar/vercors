package vct.parsers.rewrite

import vct.col.ast.`type`.{ClassType, PrimitiveSort}
import vct.col.ast.stmt.decl.{DeclarationStatement, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.AbstractRewriter

class CPPPostProcessor(source: ProgramUnit) extends AbstractRewriter(source) {

  override def visit(ct: ClassType) = {
    result = ct.getName match {
      case "std_array" =>
        create primitive_type(PrimitiveSort.Array, rewrite(ct.params.head))
      case "int32_t" =>
        create primitive_type(PrimitiveSort.Integer)
      case _ => null
    }

    if (result == null) {
      super.visit(ct)
    }
  }
}
