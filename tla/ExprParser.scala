package distcompiler.tla

import cats.syntax.all.given

import distcompiler.*
import dsl.*
import distcompiler.Builtin.{Error, SourceMarker}
import distcompiler.tla.TLAReader
import distcompiler.tla.TLAParser.rawExpression
import distcompiler.Manip.ops.pass.bottomUp
import distcompiler.tla.defns.THEOREM
import distcompiler.tla.TLAParser.RawExpression
import distcompiler.tla.TLAParser.rawConjunction

object ExprParser extends PassSeq:
  import distcompiler.dsl.*
  import distcompiler.Builtin.{Error, SourceMarker}
  import TLAReader.*
  def inputWellformed: Wellformed = TLAParser.outputWellformed
  // TODO: make private
  def highPredInfixInfix(op: defns.InfixOperator): SeqPattern[SeqPattern.Fields[Tuple1[(Node, Node, Node)]]] = 
    field(tokens.Expr.withChildren(
      field(tokens.Expr.TmpInfixGroup.withChildren(
        field(tok(defns.InfixOperator.instances*)
          .filter(op2 => 
            op2.token match
              case op2Token : defns.InfixOperator =>
                op.highPrecedence > op2Token.highPrecedence))
        ~ field(tokens.Expr)
        ~ field(tokens.Expr)
        ~ eof
      ))
      ~ eof
    ))
  def highPredInfixUnary(op: defns.InfixOperator): SeqPattern[SeqPattern.Fields[Tuple1[(Node, Node)]]] =
    field(tokens.Expr.withChildren(
      field(tokens.Expr.TmpUnaryGroup.withChildren(
        field(tok(defns.PrefixOperator.instances*)
          .filter(op2 => 
            op2.token match
              case op2Token : defns.PrefixOperator =>
                op.highPrecedence > op2Token.highPrecedence
              case op2Token : defns.PostfixOperator =>
                op.highPrecedence > op2Token.precedence))
        ~ field(tokens.Expr)
        ~ eof
      ))
      ~ eof
      ))
  def highPredUnaryInfix(op: defns.Operator): SeqPattern[SeqPattern.Fields[Tuple1[(Node, Node, Node)]]] =
    field(tokens.Expr.withChildren(
      field(tokens.Expr.TmpInfixGroup.withChildren(
        field(tok(defns.InfixOperator.instances*)
          .filter(op2 => 
            op2.token match
              case op2Token : defns.InfixOperator =>
                op.highPrecedence > op2Token.highPrecedence))
        ~ field(tokens.Expr)
        ~ field(tokens.Expr)
        ~ eof
      ))
      ~ eof
    ))

  def badPredInfixInfix(op: defns.InfixOperator): SeqPattern[SeqPattern.Fields[Tuple1[(Node, Node, Node)]]]  =
    field(tokens.Expr.withChildren(
      field(tokens.Expr.TmpInfixGroup.withChildren(
        field(tok(defns.InfixOperator.instances*)
          .filter(op2 => 
            op2.token match
              case op2Token : defns.InfixOperator =>
                !((op.highPrecedence > op2Token.highPrecedence)
                  || (op.lowPrecedence < op2Token.lowPrecedence)
                  || ((op == op2Token) && op.isAssociative))
          ))
        ~ field(tokens.Expr)
        ~ field(tokens.Expr)
        ~ eof
      ))
      ~ eof
    ))
  def badPredInfixUnary(op: defns.InfixOperator): SeqPattern[SeqPattern.Fields[Tuple1[(Node, Node)]]] =
    field(tokens.Expr.withChildren(
      field(tokens.Expr.TmpUnaryGroup.withChildren(
        field(tok(defns.PrefixOperator.instances*)
          .filter(op2 => 
            op2.token match
              case op2Token : defns.PrefixOperator =>
                !((op.highPrecedence > op2Token.highPrecedence)
                  || (op.lowPrecedence < op2Token.lowPrecedence))
              case op2Token : defns.PostfixOperator =>
                !((op.highPrecedence > op2Token.precedence)
                  || (op.lowPrecedence < op2Token.precedence))))
        ~ field(tokens.Expr)
        ~ eof
      ))
      ~ eof
    ))
  def badPredUnaryInfix(op: defns.Operator): SeqPattern[SeqPattern.Fields[Tuple1[(Node, Node, Node)]]] =
    field(tokens.Expr.withChildren(
      field(tokens.Expr.TmpInfixGroup.withChildren(
        field(tok(defns.InfixOperator.instances*)
          .filter(op2 => 
            op2.token match
              case op2Token : defns.InfixOperator =>
                !((op.highPrecedence > op2Token.highPrecedence)
                  || (op.lowPrecedence < op2Token.lowPrecedence))))
        ~ field(tokens.Expr)
        ~ field(tokens.Expr)
        ~ eof
      ))
      ~ eof
    ))
  def matchQuantifierId() :SeqPattern[(Node, TLAParser.RawExpression)] =
    parent(tokens.Expr) *>
      field(TLAReader.Alpha)
      ~ skip(defns.`\\in`)
      ~ field(rawExpression)
      ~ trailing
  def matchQuantifierIds() :SeqPattern[(List[Node], TLAParser.RawExpression)] =
    parent(tokens.Expr) *>
      field(tok(TLAReader.TupleGroup).withChildren(
        field(repeatedSepBy(`,`)(tok(TLAReader.Alpha)))
        ~ eof
        ))
      ~ skip(defns.`\\in`)
      ~ field(rawExpression)
      ~ trailing  

  // replace all tokens.Expr(contents...) with
  // tokens.Expr(tokens.ExprTry, contents...)
  
  val buildExpressions = passDef:
    wellformed := prevWellformed.makeDerived:
      val removedCases = Seq(
        TLAReader.StringLiteral,
        TLAReader.NumberLiteral,
        TLAReader.TupleGroup
        // TODO: remove cases
      )
      TLAReader.groupTokens.foreach: tok =>
        tok.removeCases(removedCases*)
        tok.addCases(tokens.Expr)
      
      tokens.Expr.TmpInfixGroup ::= fields(
        choice(defns.InfixOperator.instances*),
        tokens.Expr,
        tokens.Expr
      )
      tokens.Expr.TmpUnaryGroup ::= fields(
        choice((defns.PrefixOperator.instances ++ defns.PostfixOperator.instances)*),
        tokens.Expr,
      )

      tokens.Expr.deleteShape()
      tokens.Expr.importFrom(tla.wellformed)
      tokens.Expr.addCases(
        tokens.Expr,
        tokens.Expr.TmpInfixGroup,
        tokens.Expr.TmpUnaryGroup)

    // TODO: assign the correct source with .like()

    pass(once = false, strategy = pass.bottomUp) // conjunction alignment
      .rules:
        on(
          parent(tokens.Expr) *>
            (field(tok(defns./\))
              ~ field(rawConjunction(1))
              ~ field(tok(defns./\))
              ~ field(rawConjunction(1))
              ~ eof
            ).filter(things =>
              things match
                case (and1 : Node, r1, and2 : Node, r2) =>
                  val s1 = and1.sourceRange
                  val s2 = and2.sourceRange
                  // println("\nCol 1: " + s1.source.lines.lineColAtOffset(s1.offset))
                  // println("\nCol 2: " + s2.source.lines.lineColAtOffset(s2.offset))
                  s1.source.lines.lineColAtOffset(s1.offset)._2 == s2.source.lines.lineColAtOffset(s2.offset)._2
              )
        ).rewrite: (and1, r1, and2, r2) =>
          splice(
            and1.unparent(),
            tokens.Expr(
              tokens.Expr.OpCall(
                tokens.OpSym(and2.unparent()),
                tokens.Expr.OpCall.Params(
                  r1.mkNode,
                  r2.mkNode
                ))))
    *> pass(once = false, strategy = pass.bottomUp) // remove leading /\, remove paren
      .rules:
        // on(
        //   field(tokens.Expr.withChildren(
        //     skip(defns./\)
        //     ~ field(tokens.Expr)
        //     ~ eof
        //   ))
        //   ~ eof
        // ).rewrite: expr =>
        //    splice(expr.unparent())
        on(
          field(tokens.Expr.withChildren(
            skip(defns./\)
            ~ field(rawExpression)
            ~ eof
          ))
          ~ eof
        ).rewrite: expr =>
           splice(expr.mkNode)
    *> pass(once = false, strategy = pass.bottomUp) // resolve quantifiers/opCall
      .rules:
        on(
          parent(tokens.Expr) *> 
            field(TLAReader.Alpha)
            ~ field(TLAReader.ParenthesesGroup.withChildren(
                field(repeatedSepBy(`,`)(rawExpression))
                ~ eof
                ))
            ~ trailing
        ).rewrite: (fun, args) =>
          splice(
            tokens.Expr(tokens.Expr.OpCall(
              tokens.Id().like(fun),
              tokens.Expr.OpCall.Params(
                args.iterator.map(_.mkNode)
              ))))
        | on(
          parent(tokens.Expr) *>
            skip(tok(TLAReader.LaTexLike).src("\\E"))
            ~ field(repeatedSepBy1(`,`)(matchQuantifierId()))
            ~ skip(TLAReader.`:`)
            ~ field(rawExpression)
            ~ trailing
        ).rewrite: (qBounds, expr) =>
            splice(
              tokens.Expr(tokens.Expr.Exists(
                tokens.QuantifierBounds(
                  qBounds.iterator.map(
                    (id, qExpr) =>
                      tokens.QuantifierBound(
                        tokens.Id().like(id),
                        qExpr.mkNode
                      ))),
                expr.mkNode
              )))
        | on(
          parent(tokens.Expr) *>
            skip(tok(TLAReader.LaTexLike).src("\\E"))
            ~ field(repeatedSepBy1(`,`)(matchQuantifierIds()))
            ~ skip(TLAReader.`:`)
            ~ field(rawExpression)
            ~ trailing
        ).rewrite: (qBounds, expr) =>
            splice(
              tokens.Expr(tokens.Expr.Exists(
                tokens.QuantifierBounds(
                  qBounds.iterator.map(
                    (ids, qExpr) =>
                      tokens.QuantifierBound(
                        tokens.Ids(
                          ids.iterator.map(
                            id => tokens.Id().like(id)
                          )
                        ),
                        qExpr.mkNode
                      ))),
                expr.mkNode
              )))
        | on(
          parent(tokens.Expr) *>
            skip(tok(TLAReader.LaTexLike).src("\\A"))
            ~ field(repeatedSepBy1(`,`)(matchQuantifierId()))
            ~ skip(TLAReader.`:`)
            ~ field(rawExpression)
            ~ trailing
        ).rewrite: (qBounds, expr) =>
            splice(
              tokens.Expr(tokens.Expr.Forall(
                tokens.QuantifierBounds(
                  qBounds.iterator.map(
                    (id, qExpr) =>
                      tokens.QuantifierBound(
                        tokens.Id().like(id),
                        qExpr.mkNode
                      ))),
                expr.mkNode
              )))
        | on(
          parent(tokens.Expr) *>
            skip(tok(TLAReader.LaTexLike).src("\\A"))
            ~ field(repeatedSepBy1(`,`)(matchQuantifierIds()))
            ~ skip(TLAReader.`:`)
            ~ field(rawExpression)
            ~ trailing
        ).rewrite: (qBounds, expr) =>
            splice(
              tokens.Expr(tokens.Expr.Forall(
                tokens.QuantifierBounds(
                  qBounds.iterator.map(
                    (ids, qExpr) =>
                      tokens.QuantifierBound(
                        tokens.Ids(
                          ids.iterator.map(
                            id => tokens.Id().like(id)
                          )
                        ),
                        qExpr.mkNode
                      ))),
                expr.mkNode
              )))
        | on(
          parent(tokens.Expr) *>
            skip(tok(defns.CHOOSE))
            ~ field(matchQuantifierId())
            ~ skip(TLAReader.`:`)
            ~ field(rawExpression)
            ~ trailing
        ).rewrite: (qBound, expr) =>
            qBound match
              case (id, qExpr) =>
                splice(
                  tokens.Expr(tokens.Expr.Choose(
                    tokens.QuantifierBound(
                      tokens.Id().like(id),
                      qExpr.mkNode
                    ),
                    expr.mkNode
                  )))
    // TODO: tuple qbound
    //       id nil
    //       tuple nil
    *> pass(once = false, strategy = pass.bottomUp)
      .rules:
        on(
          parent(tokens.Expr) *>
            TLAReader.Alpha
        ).rewrite: name =>
          splice(
            tokens.Expr(
              tokens.Id().like(name)
            ))
        | on(
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
                field(
                  repeatedSepBy1(`,`)(
                    field(TLAReader.Alpha)
                      ~ skip(`|->`)
                      ~ field(rawExpression)
                      ~ trailing
                  ))
                ~ eof
        ).rewrite: fields =>
          splice(
            tokens.Expr(
              tokens.Expr.RecordLiteral(
                fields.iterator.map(
                  (alpha, expr) =>
                    tokens.Expr.RecordLiteral.Field(
                      tokens.Id().like(alpha.unparent()),
                      expr.mkNode
                    )))))
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr)
            ~ skip(defns.`.`)
            ~ field(tokens.Expr.withChildren(
              field(tokens.Id)
              ~ eof
            ))
            ~ trailing
        ).rewrite: (expr, id) =>
          splice(
            tokens.Expr(
              tokens.Expr.Project(
                tokens.Expr(
                  expr.unparent()
                ),
                id.unparent()
              )))
        | on(
          parent(tokens.Expr) *>
            tok(TLAReader.SqBracketsGroup) *>
              children:
                field(
                  repeatedSepBy1(`,`)(
                    field(TLAReader.Alpha)
                      ~ skip(`:`)
                      ~ field(rawExpression)
                      ~ trailing
                  ))
                ~ eof
        ).rewrite: fields =>
          splice(
            tokens.Expr(
              tokens.Expr.RecordSetLiteral(
                fields.iterator.map(
                  (alpha, expr) =>
                    tokens.Expr.RecordSetLiteral.Field(
                      tokens.Id().like(alpha.unparent()),
                      expr.mkNode
                    )))))
        | on( 
          parent(tokens.Expr) *>
            field(tokens.Expr)
            ~ field(tok(defns.InfixOperator.instances.filter(_ != defns.`.`)*))
            ~ field(tokens.Expr)
            ~ eof
        ).rewrite: (left, op, right) =>
          splice(
            tokens.Expr(tokens.Expr.TmpInfixGroup(
              op.unparent(),
              left.unparent(),
              right.unparent()
            )))
        | on(
          parent(tokens.Expr) *>
          field(tok(defns.PrefixOperator.instances*))
          ~ field(tokens.Expr)
          ~ eof
        ).rewrite: (op, expr) =>
          splice(
            tokens.Expr(tokens.Expr.TmpUnaryGroup(
              op.unparent(),
              expr.unparent(),
            )))
        | on(
          parent(tokens.Expr) *>
          field(tokens.Expr)
          ~ field(tok(defns.PostfixOperator.instances*))
          ~ trailing
        ).rewrite: (expr, op) =>
          splice(
            tokens.Expr(tokens.Expr.TmpUnaryGroup(
              op.unparent(),
              expr.unparent(),
            )))
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr)
            ~ field(tok(TLAReader.SqBracketsGroup) *>
              children:
                field(repeatedSepBy(`,`)(rawExpression))
                ~ eof
                )
            ~ eof 
        ).rewrite: (callee, args) =>
          splice(
            tokens.Expr(tokens.Expr.FnCall(
              callee.unparent(),
              args match
                case List(expr) =>
                  expr.mkNode
                case _ =>
                  tokens.Expr(tokens.Expr.TupleLiteral(
                    args.iterator.map(_.mkNode)
                  )))))
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
            ~ field(
                optional(
                  skip(defns.OTHER)
                  ~ skip(defns.-)
                  ~ skip(defns.>)
                  ~ field(tokens.Expr)
                  ~ eof
                )
              )
            ~ eof
        ).rewrite: (cases, other) =>
          splice(
            tokens.Expr(
              tokens.Expr.Case(
                tokens.Expr.Case.Branches(
                  cases.iterator.map((pred, branch) =>
                    tokens.Expr.Case.Branch(
                      pred.unparent(),
                      branch.unparent(),
                    ))),
                tokens.Expr.Case.Other(
                  other match
                    case None => tokens.Expr.Case.Other.None()
                    case Some(expr) => expr.unparent() 
                )
              )))
        | on(
          parent(tokens.Expr) *>
            field(tok(TLAReader.LetGroup).product(
              children(
                field(
                  repeated1:
                    tok(tokens.Operator)
                      | tok(tokens.ModuleDefinition)
                      | tok(tokens.Recursive)
                ) 
                ~ eof
              )
            ))
            ~ field(tokens.Expr)
            ~ trailing
        ).rewrite: (let, expr)  =>
            splice(
              tokens.Expr(
                tokens.Expr.Let(
                  tokens.Expr.Let.Defns(
                    let._2.iterator.map(_.unparent())
                  ),
                  expr.unparent()
                )).like(let._1)
            )
        // TODO: Function
        // TODO: SetComprehension {expr : x \in y}
        // TODO: SetRefinement   {x \in y : bool} 
        // {x \in y : p \in q} TODO: look in the book
        // TODO: Choose
        // TODO: Except
        // TODO: Lambda
        // TODO a \x b \ a
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
            tok(TLAReader.ParenthesesGroup) *>
              children:
                field(rawExpression)
                ~ eof
        ).rewrite: rawExpr =>
          splice(
            tokens.Expr(rawExpr.mkNode)
          )
    *> pass(once = false, strategy = pass.bottomUp) // resolve Alphas
      .rules:
        on(
          parent(tokens.Expr) *>
            tokens.Id
        ).rewrite: name =>
          splice(
            tokens.Expr(
              tokens.Expr.OpCall(
                name.unparent(),
                tokens.Expr.OpCall.Params(),
              )))
  end buildExpressions

  val reorderOperations = passDef:
    wellformed := prevWellformed.makeDerived:
      tokens.Expr.removeCases(
        tokens.Expr.TmpInfixGroup,
        tokens.Expr.TmpUnaryGroup)
    pass(once = false, strategy = pass.bottomUp)
      .rules:
        on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpInfixGroup.withChildren:
                defns.InfixOperator.instances
                  .iterator
                  .map: op =>
                    field(op)
                    ~ highPredInfixInfix(op)
                    ~ field(tokens.Expr)
                    ~ eof
                  .reduce(_ | _))
            ~ eof
        ).rewrite: (curOp, left, right) =>
          left match
            case (op, expr1, expr2) =>
              splice(
                tokens.Expr.TmpInfixGroup(
                  op.unparent(),
                  expr1.unparent(),
                  tokens.Expr(tokens.Expr.TmpInfixGroup(
                    curOp.unparent(),
                    expr2.unparent(),
                    right.unparent()
                  ))))
        |  on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpInfixGroup.withChildren:
                defns.InfixOperator.instances
                  .iterator
                  .map: op =>
                    field(op)
                    ~ field(tokens.Expr)
                    ~ highPredInfixInfix(op)
                    ~ eof
                  .reduce(_ | _))
            ~ eof
        ).rewrite: (curOp, left, right) =>
          right match
            case (op, expr1, expr2) =>
              splice(
                tokens.Expr.TmpInfixGroup(
                  op.unparent(),
                  tokens.Expr(tokens.Expr.TmpInfixGroup(
                    curOp.unparent(),
                    left.unparent(),
                    expr1.unparent()
                  )),
                  expr2.unparent()))
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpInfixGroup.withChildren:
                defns.InfixOperator.instances
                  .iterator
                  .map: op =>
                    field(op)
                    ~ highPredInfixUnary(op)
                    ~ field(tokens.Expr)
                    ~ eof
                  .reduce(_ | _))
            ~ eof
        ).rewrite: (curOp, left, right) =>
          left match
            case (op, expr) =>
              splice(
                tokens.Expr.TmpUnaryGroup(
                  op.unparent(),
                  tokens.Expr(tokens.Expr.TmpInfixGroup(
                    curOp.unparent(),
                    expr.unparent(),
                    right.unparent()
                  ))))
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpInfixGroup.withChildren:
                defns.InfixOperator.instances
                  .iterator
                  .map: op =>
                    field(op)
                    ~ field(tokens.Expr)
                    ~ highPredInfixUnary(op)
                    ~ eof
                  .reduce(_ | _))
            ~ eof
        ).rewrite: (curOp, left, right) =>
          right match
            case (op, expr) =>
              splice(
                tokens.Expr.TmpUnaryGroup(
                  op.unparent(),
                  tokens.Expr(tokens.Expr.TmpInfixGroup(
                    curOp.unparent(),
                    left.unparent(),
                    expr.unparent()
                  ))))
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpUnaryGroup.withChildren:
                defns.PrefixOperator.instances
                  .iterator
                  .map: op =>
                    field(op)
                    ~ highPredUnaryInfix(op)
                    ~ eof
                  .reduce(_ | _))
            ~ eof
        ).rewrite: (curOp, infixGroup) =>
          infixGroup match
            case (op, left, right) =>
              splice(
                tokens.Expr.TmpInfixGroup(
                  op.unparent(),
                  tokens.Expr(tokens.Expr.TmpUnaryGroup(
                    curOp.unparent(),
                    left.unparent()
                  )),
                  right.unparent()))
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpUnaryGroup.withChildren:
                defns.PostfixOperator.instances
                  .iterator
                  .map: op =>
                    field(op)
                    ~ highPredUnaryInfix(op)
                    ~ eof
                  .reduce(_ | _))
            ~ eof
        ).rewrite: (curOp, infixGroup) =>
          infixGroup match
            case (op, left, right) =>
              splice(
                tokens.Expr.TmpInfixGroup(
                  op.unparent(),
                  left.unparent(),
                  tokens.Expr(tokens.Expr.TmpUnaryGroup(
                    curOp.unparent(),
                    right.unparent()
                  ))))
    *> pass(once = false, strategy = pass.bottomUp) // assoc related errors
      .rules:
        on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpInfixGroup.withChildren:
                defns.InfixOperator.instances
                  .iterator
                  .map: op =>
                    field(op)
                    ~ badPredInfixInfix(op)
                    ~ field(tokens.Expr)
                    ~ eof
                  .reduce(_ | _))
            ~ eof
        ).rewrite: (curOp, left, right) =>
          left match
            case (op, expr1, expr2) =>
              splice(
                Node(Builtin.Error)(
                  Builtin.Error.Message( // todo: use src
                    s"$curOp and $op must have different precedence, or be duplicates of an associative operator."
                  ),
                  Builtin.Error.AST(
                    tokens.Expr.TmpInfixGroup(
                      curOp.unparent(),
                      tokens.Expr.TmpInfixGroup(
                        op.unparent(),
                        expr1.unparent(),
                        expr2.unparent()
                      ),
                      right.unparent()))))
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpInfixGroup.withChildren:
                defns.InfixOperator.instances
                  .iterator
                  .map: op =>
                    field(op)
                    ~ field(tokens.Expr)
                    ~ badPredInfixInfix((op))
                    ~ eof
                  .reduce(_ | _))
            ~ eof
        ).rewrite: (curOp, left, right) =>
          right match
            case (op, expr1, expr2) =>
              splice(
                Node(Builtin.Error)(
                  Builtin.Error.Message(
                    s"$curOp and $op must have different precedence, or be duplicates of an associative operator."
                  ),
                  Builtin.Error.AST(
                    tokens.Expr.TmpInfixGroup(
                      curOp.unparent(),
                      left.unparent(),
                      tokens.Expr.TmpInfixGroup(
                        op.unparent(),
                        expr1.unparent(),
                        expr2.unparent()
                      )))))
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpInfixGroup.withChildren:
                defns.InfixOperator.instances
                  .iterator
                  .map: op =>
                    field(op)
                    ~ badPredInfixUnary(op)
                    ~ field(tokens.Expr)
                    ~ eof
                  .reduce(_ | _))
            ~ eof
        ).rewrite: (curOp, left, right) =>
          left match
            case (op, expr) =>
              splice(
                Node(Builtin.Error)(
                  Builtin.Error.Message(
                    s"$curOp and $op must have different precedence."
                  ),
                  Builtin.Error.AST(
                    tokens.Expr.TmpInfixGroup(
                      curOp.unparent(),
                      tokens.Expr.TmpUnaryGroup(
                        op.unparent(),
                        expr.unparent(),
                      )),
                      right.unparent()
                    )))
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpInfixGroup.withChildren:
                defns.InfixOperator.instances
                  .iterator
                  .map: op =>
                    field(op)
                    ~ field(tokens.Expr)
                    ~ badPredInfixUnary(op)
                    ~ eof
                  .reduce(_ | _))
            ~ eof
        ).rewrite: (curOp, left, right) =>
          right match
            case (op, expr) =>
              splice(
                Node(Builtin.Error)(
                  Builtin.Error.Message(
                    s"$curOp and $op must have different precedence."
                  ),
                  Builtin.Error.AST(
                    tokens.Expr.TmpInfixGroup(
                      curOp.unparent(),
                      left.unparent(),
                      tokens.Expr.TmpUnaryGroup(
                        op.unparent(),
                        expr.unparent()
                      )))))
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpUnaryGroup.withChildren:
                defns.PrefixOperator.instances
                  .iterator
                  .map: op =>
                    field(op)
                    ~ badPredUnaryInfix(op)
                    ~ eof
                  .reduce(_ | _))
            ~ eof
        ).rewrite: (curOp, infixGroup) =>
          infixGroup match
            case (op, left, right) =>
              splice(
                Node(Builtin.Error)(
                  Builtin.Error.Message(
                    s"$curOp and $op must have different precedence."
                  ),
                  Builtin.Error.AST(
                    tokens.Expr.UnaryGroup(
                      curOp.unparent(),
                      tokens.Expr.TmpInfixGroup(
                        op.unparent(),
                        left.unparent(),
                        right.unparent()
                      )))))
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpUnaryGroup.withChildren:
                defns.PostfixOperator.instances
                  .iterator
                  .map: op =>
                    field(op)
                    ~ badPredUnaryInfix(op)
                    ~ eof
                  .reduce(_ | _))
            ~ eof
        ).rewrite: (curOp, infixGroup) =>
          infixGroup match
            case (op, left, right) =>
              splice(
                Node(Builtin.Error)(
                  Builtin.Error.Message(
                    s"$curOp and $op must have different precedence."
                  ),
                  Builtin.Error.AST(
                    tokens.Expr.UnaryGroup(
                      curOp.unparent(),
                      tokens.Expr.TmpInfixGroup(
                        op.unparent(),
                        left.unparent(),
                        right.unparent()
                      )))))
    *> pass(once = false, strategy = pass.bottomUp)
      .rules:
        on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpInfixGroup.withChildren(
              field(tok(defns.InfixOperator.instances*))
              ~ field(tokens.Expr)
              ~ field(tokens.Expr)
              ~ eof
            ))
            ~ eof
        ).rewrite: (op, right, left) =>
          splice(
            tokens.Expr(tokens.Expr.OpCall(
              tokens.OpSym(op.unparent()),
              tokens.Expr.OpCall.Params(
                right.unparent(),
                left.unparent()
              ))))
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpUnaryGroup.withChildren(
              field(tok(defns.PrefixOperator.instances*) 
                | tok(defns.PostfixOperator.instances*))
              ~ field(tokens.Expr)
              ~ eof
            ))
            ~ eof
        ).rewrite: (op, expr) =>
          splice(
            tokens.Expr(tokens.Expr.OpCall(
              tokens.OpSym(op.unparent()),
              tokens.Expr.OpCall.Params(
                expr.unparent(),
              ))))
  end reorderOperations

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
