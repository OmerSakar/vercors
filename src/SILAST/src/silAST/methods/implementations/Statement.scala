package silAST.methods.implementations

import silAST.ASTNode
import silAST.types.DataType
import silAST.expressions.Expression
import silAST.expressions.PredicateExpression
import silAST.source.SourceLocation
import silAST.expressions.util.PTermSequence
import silAST.programs.symbols.{ProgramVariableSequence, Field, ProgramVariable}
import silAST.methods.Method
import silAST.expressions.terms.PTerm

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
sealed abstract class Statement private[silAST] extends ASTNode {
  override def toString: String
}

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
final case class AssignmentStatement private[silAST](
                                                      sourceLocation: SourceLocation,
                                                      target: ProgramVariable,
                                                      source: PTerm
                                                      )
  extends Statement {
  override def toString: String = target.name + ":=" + source.toString
}

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
case class FieldAssignmentStatement private[silAST](
                                                     sourceLocation: SourceLocation,
                                                     target: ProgramVariable,
                                                     field: Field,
                                                     source: PTerm
                                                     )
  extends Statement {
  override def toString: String = target.name + "." + field.name + " := " + source.toString
}

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
case class NewStatement private[silAST](
                                         sourceLocation: SourceLocation,
                                         target: ProgramVariable,
                                         dataType: DataType
                                         )
  extends Statement {
  override def toString: String = target.name + ":= new " + dataType.toString
}

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//TODO:check signature
final case class CallStatement private[silAST]
(
  sourceLocation: SourceLocation,
  targets: ProgramVariableSequence,
  receiver: PTerm,
  method: Method,
  arguments: PTermSequence
  )
  extends Statement {
  override def toString: String = targets.toString + " := " + receiver.toString + "." + method.name + arguments.toString
}

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
final case class InhaleStatement private[silAST](
                                                  sourceLocation: SourceLocation,
                                                  expression: Expression
                                                  )
  extends Statement {
  override def toString: String = "inhale " + expression.toString
}

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
final case class ExhaleStatement private[silAST](
                                                  sourceLocation: SourceLocation,
                                                  expression: Expression
                                                  )
  extends Statement {
  override def toString: String = "exhale " + expression.toString
}

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//TODO:FoldStatement/UnfoldStatement arrays?
final case class FoldStatement private[silAST](
                                                sourceLocation: SourceLocation,
                                                predicate: PredicateExpression
                                                )
  extends Statement {
  override def toString: String = "fold " + predicate.toString
}

//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////
final case class UnfoldStatement private[silAST](
                                                  sourceLocation: SourceLocation,
                                                  predicate: PredicateExpression
                                                  )
  extends Statement {
  override def toString: String = "unfold " + predicate.toString
}
