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

    // TODO: assign the correct source with .like()
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
                repeatedSepBy1(`,`)(
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
        // TOOD: projection should match before Names are assigned 
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr.withChildren(
              field(tokens.Expr.OpCall)
              ~ eof
            )) // TODO: can this be a function call? as in, can param be non empty?
            ~ skip(tok(defns.`.`))
            ~ field(tokens.Expr.withChildren(
                field(tokens.Expr.OpCall.withChildren(
                  field(tokens.Id)
                  ~ skip(tokens.Expr.OpCall.Params) // TODO:  these have to be empty
                  ~ eof
                ))
                ~ eof
              ))
            ~ trailing
        ).rewrite: (id1, id2) =>
          splice(
            tokens.Expr(
              tokens.Expr.Project(
                tokens.Expr(
                  id1.unparent()
                ),
                id2.unparent()
              )))
        | on( // TODO: is this even legal?
          parent(tokens.Expr) *>
            field(tokens.Expr) // TODO: ensure this is a record?
            ~ skip(tok(defns.`.`))
            ~ field(tokens.Expr.withChildren(
                field(tokens.Expr.OpCall.withChildren(
                  field(tokens.Id)
                  ~ skip(tokens.Expr.OpCall.Params) // TODO:  these have to be empty
                  ~ eof
                ))
                ~ eof
              ))
            ~ trailing
        ).rewrite: (expr, id) =>
          splice(
            tokens.Expr(
              tokens.Expr.Project(
                expr.unparent(),
                id.unparent()
              )))
        // TODO: second project
        // TODO: recordSetLiteral
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
        // TODO: calls
        | on( 
          parent(tokens.Expr) *>
            field(tokens.Expr)
            // TODO: proper filter
            ~ field(tok(defns.InfixOperator.instances.filter(i => i.highPredecence < 17)*))
            ~ field(tokens.Expr)
            ~ trailing
        ).rewrite: (left, op, right) =>
          splice(
            tokens.Expr(
              tokens.Expr.OpCall(
                  tokens.OpSym(op.unparent()),
                  tokens.Expr.OpCall.Params(left.unparent(), right.unparent())
                )
                .like(op)
            ))
        // ~ field(tok(defns.InfixOperator.instances.filter(_.isAssociative)*))
        // TODO: prefix
        // TODO: infix assoc
        // TODO: infix non assoc
        // TODO: Fn Call
        | on(
          parent(tokens.Expr) *>
            skip(defns.IF)
            ~ field(tokens.Expr)
            ~ skip(defns.THEN)
            ~ field(tokens.Expr)
            ~ skip(defns.ELSE)
            ~ field(tokens.Expr)
            ~ trailing
        ).rewrite: (pred, t, f) =>
          splice(
            tokens.Expr.If(
              pred.unparent(),
              t.unparent(),
              f.unparent()
            ))
        | on(
          parent(tokens.Expr) *>
            skip(defns.CASE)
            ~ field(
              repeatedSepBy(defns.`[]`)(
                field(tokens.Expr)
                ~ skip(defns.-)
                ~ skip(defns.>)
                ~ field(tokens.Expr)
                ~ trailing
                ))
            ~ eof
            // TODO: optional OTHER
        ).rewrite: cases =>
          splice(
            tokens.Expr(
              tokens.Expr.Case(
                cases.iterator.map((pred, branch) =>
                  tokens.Expr.Case.Branch(
                    pred.unparent(),
                    branch.unparent(),
                  )))))
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
