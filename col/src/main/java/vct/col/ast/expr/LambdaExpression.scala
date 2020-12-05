package vct.col.ast.expr

import hre.lang.System.Debug
import vct.col.ast.`type`.Type
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.DeclarationStatement
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

case class LambdaExpression(returnType: Type, args: Seq[DeclarationStatement], body: ASTNode, name: Option[NameExpression]) extends ExpressionNode with VisitorHelper {

  def this(returnType: Type, args: Seq[DeclarationStatement], body: ASTNode) = this(returnType, args, body, None)


  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields(): Iterable[String] = Seq("returnType", "args", "body", "name")
  override def debugTreePropertyFields(): Iterable[String] = Seq()



}
