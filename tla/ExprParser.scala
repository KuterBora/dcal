package distcompiler.tla

import cats.syntax.all.given

import distcompiler.*
import dsl.*
import distcompiler.Builtin.{Error, SourceMarker}
import distcompiler.tla.TLAReader
import distcompiler.tla.TLAParser.rawExpression
import distcompiler.Manip.ops.pass.bottomUp

object ExprParser extends PassSeq:
  import distcompiler.dsl.*
  import distcompiler.Builtin.{Error, SourceMarker}
  import TLAReader.*
  def inputWellformed: Wellformed = TLAParser.outputWellformed

  val buildExpressions = passDef:
    wellformed := prevWellformed.makeDerived:
      val removedCases = Seq(
        TLAReader.StringLiteral,
        TLAReader.NumberLiteral,
        TLAReader.TupleGroup
      )
      TLAReader.groupTokens.foreach: tok =>
        tok.removeCases(removedCases*)
        tok.addCases(tokens.Expr)
      
      tokens.Expr.deleteShape()
      tokens.Expr.importFrom(tla.wellformed)
      tokens.Expr.addCases(tokens.Expr)

    pass(once = false, strategy = pass.bottomUp)
      .rules:
        on(
           parent(tokens.Expr) *>
            TLAReader.NumberLiteral
        ).rewrite: lit =>
          splice(tokens.Expr(tokens.Expr.NumberLiteral().like(lit)))
        | on(
          parent(tokens.Expr) *>
            TLAReader.StringLiteral
        ).rewrite: lit =>
          splice(tokens.Expr(tokens.Expr.StringLiteral().like(lit)))
        | on(
          parent(tokens.Expr) *>
            tok(TLAReader.BracesGroup) *>
              children:
                field(repeatedSepBy(`,`)(rawExpression))
                ~ eof
        ).rewrite: exprs =>
          splice(
            tokens.Expr(
              tokens.Expr.SetLiteral(exprs.iterator.map(_.mkNode))
            ))
        | on(
          parent(tokens.Expr) *>
            tok(TLAReader.TupleGroup).product(
              children(
                field(repeatedSepBy(`,`)(rawExpression))
                  ~ eof
              ))
        ).rewrite: (lit, elems) =>
          splice(
            tokens.Expr(
              tokens.Expr.TupleLiteral(elems.iterator.map(_.mkNode)).like(lit)
            ))
        | on(
          parent(tokens.Expr) *>
            tok(TLAReader.SqBracketsGroup) *>
              children:
                repeatedSepBy(`,`)(
                  field(TLAReader.Alpha)
                    ~ skip(`|->`)
                    ~ field(rawExpression)
                    ~ trailing
                )
        ).rewrite: fields =>
          splice(
            tokens.Expr(
              tokens.Expr.RecordLiteral(
                fields.iterator.map(
                  (opCall, expr) =>
                    tokens.Expr.RecordLiteral.Field(
                      tokens.Id().like(opCall.unparent()),
                      expr.mkNode
                    ) 
                ))))
        // TODO: Project
        | on(
          parent(tokens.Expr) *>
          TLAReader.Alpha
        ).rewrite: name =>
          splice(
            tokens.Expr(
              tokens.Expr.OpCall(
                tokens.Id().like(name),
                tokens.Expr.OpCall.Params(),
              )))
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr)
            ~ field(tok(defns.InfixOperator.instances*))
            ~ field(tokens.Expr)
            ~ trailing
        ).rewrite: (left, op, right) =>
          splice(
            tokens.Expr(
              tokens.Expr
                .OpCall(
                  tokens.OpSym(op.unparent()),
                  tokens.Expr.OpCall.Params(left.unparent(), right.unparent())
                )
                .like(op)
            ))
        // ~ field(tok(defns.InfixOperator.instances.filter(_.isAssociative)*))
        // TODO: other kinds of OpCalls
        // TODO: Fn Call
        // TODO: if
        // TODO: Case
        // TODO: Let
        // TODO: Exists
        // TODO: Forall
        // TODO: Function
        // TODO: SetComprehension
        // TODO: SetRefinement
        // TODO: Choose
        // TODO: Except
        // TODO: Lambda
        | on(
          parent(tokens.Expr) *>
            tok(TLAReader.ParenthesesGroup) *>
              children:
                field(rawExpression)
                ~ eof
        ).rewrite: expr =>
          splice(
            tokens.Expr(expr.mkNode)
          )
  end buildExpressions

  val removeNestedExpr = passDef:
    wellformed := prevWellformed.makeDerived:
      tokens.Expr.removeCases(tokens.Expr)

    pass(once = true, strategy = bottomUp)
      .rules:
        on(
          tok(tokens.Expr) *>
            onlyChild(tokens.Expr)
        ).rewrite: child =>
          splice(
            child.unparent()
          )
  end removeNestedExpr
