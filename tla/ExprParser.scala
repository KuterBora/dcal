package distcompiler.tla

import cats.syntax.all.given

import distcompiler.*
import dsl.*
import distcompiler.Builtin.{Error, SourceMarker}
import distcompiler.tla.TLAReader
import distcompiler.tla.TLAParser.rawExpression
import distcompiler.Manip.ops.pass.bottomUp
import distcompiler.tla.defns.THEOREM

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

      tokens.Expr.deleteShape()
      tokens.Expr.importFrom(tla.wellformed)
      tokens.Expr.addCases(tokens.Expr, tokens.Id, tokens.Expr.TmpInfixGroup) 

    // TODO: assign the correct source with .like()
    pass(once = false, strategy = pass.bottomUp)
      .rules:
        on(
          parent(tokens.Expr) *>
            TLAReader.Alpha
        ).rewrite: name =>
          splice(
            // TODO???
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
        // TODO: all OpCall
        | on( 
          parent(tokens.Expr) *>
            field(tokens.Expr)
            ~ field(tok(defns.InfixOperator.instances.filter(i => i.highPrecedence < 17)*)) // TODO: probably dont do this filter
            ~ field(tokens.Expr)
            ~ eof // eof here?
        ).rewrite: (left, op, right) =>
          println("\nFound Op: " + op + "Rewriting as: \n")
          println(tokens.Expr(tokens.Expr.TmpInfixGroup(
              op.unparent(),
              left.unparent(),
              right.unparent()
            )))
          splice(
            tokens.Expr(tokens.Expr.TmpInfixGroup(
              op.unparent(),
              left.unparent(),
              right.unparent()
            )))
        // TODO: Fn Call
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
                    tok(tokens.Operator) // ???: is this correct usage of |
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
        // ???: Exists and Forall
        // | on(
        //   parent(tokens.Expr) *>
        //     field(TLAReader.LaTexLike)
        //     ~ field(tokens.Expr)
        //     ~ field(TLAReader.`:`)
        //     ~ field(tokens.Expr)
        //     ~ eof
        // ).rewrite: thing =>
        //     ???
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
  end buildExpressions

  val orderOperations = passDef:
    wellformed := prevWellformed.makeDerived:
      tokens.Expr.TmpInfixGroup.Checked ::= fields(
        choice(defns.InfixOperator.instances*),
        tokens.Expr,
        tokens.Expr
      )
      tokens.Expr.removeCases(tokens.Expr.TmpInfixGroup)
      tokens.Expr.addCases(tokens.Expr.TmpInfixGroup.Checked) 
    
    pass(once = false, strategy = pass.bottomUp)
      .rules:
        on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpInfixGroup.withChildren(
              field(tok(defns.InfixOperator.instances*))
              ~ field(tokens.Expr.withChildren(
                field(tokens.Expr.TmpInfixGroup.Checked.withChildren(
                  field(tok(defns.InfixOperator.instances*))
                  ~ field(tokens.Expr)
                  ~ field(tokens.Expr)
                  ~ eof
                ))
                ~ eof
              ))
              ~ field(tokens.Expr)
              ~ eof
            ))
            ~ eof
        ).rewrite: (cur_op, left, right) =>
          left match
            case (op, expr1, expr2) =>
              (cur_op.token, op.token) match
                case (cur_tok: defns.InfixOperator, child_tok: defns.InfixOperator) =>
                  if (cur_tok.highPrecedence > child_tok.highPrecedence) {
                    println("\nbad order: rewriting left in: ")
                    println("\ncur_op: " + cur_op)
                    println("\nleft: " + left)
                    println("\nright: " + right)
                    println("\nas: " +
                      tokens.Expr.TmpInfixGroup(
                        op.unparent(),
                        expr1.unparent(),
                        tokens.Expr(tokens.Expr.TmpInfixGroup(
                          cur_op.unparent(),
                          expr2.unparent(),
                          right.unparent()
                        ))))
                    splice(
                      tokens.Expr.TmpInfixGroup(
                        op.unparent(),
                        expr1.unparent(),
                        tokens.Expr(tokens.Expr.TmpInfixGroup(
                          cur_op.unparent(),
                          expr2.unparent(),
                          right.unparent()
                        ))))
                  } else if (cur_tok.lowPrecedence < child_tok.lowPrecedence) {
                    println("\ngood order: Checked Left")
                    splice(
                      tokens.Expr.TmpInfixGroup.Checked(
                        cur_op.unparent(),
                        tokens.Expr(tokens.Expr.TmpInfixGroup.Checked(
                          op.unparent(),
                          expr1.unparent(),
                          expr2.unparent()
                        )),
                        right.unparent()
                      ))
                  } else if (cur_tok.isAssociative && child_tok.isAssociative) {
                    println("\nassoc found: Checked Left")
                    splice(
                      tokens.Expr.TmpInfixGroup.Checked(
                        cur_op.unparent(),
                        tokens.Expr(tokens.Expr.TmpInfixGroup.Checked(
                          op.unparent(),
                          expr1.unparent(),
                          expr2.unparent()
                        )),
                        right.unparent()
                      ))
                  } else {
                    println("\nError")
                    splice(tokens.Expr.SomeKindOfError())
                  }
        | on(
          parent(tokens.Expr) *>
            field(tokens.Expr.TmpInfixGroup.withChildren(
              field(tok(defns.InfixOperator.instances*))
              ~ field(tokens.Expr)
              ~ field(tokens.Expr.withChildren(
                field(tokens.Expr.TmpInfixGroup.Checked.withChildren(
                  field(tok(defns.InfixOperator.instances*))
                  ~ field(tokens.Expr)
                  ~ field(tokens.Expr)
                  ~ eof
                ))
                ~ eof
              ))
              ~ eof
            ))
            ~ eof
        ).rewrite: (cur_op, left, right) =>
          right match
            case (op, expr1, expr2) =>
              (cur_op.token, op.token) match
                case (cur_tok: defns.InfixOperator, child_tok: defns.InfixOperator) =>
                  if (cur_tok.highPrecedence > child_tok.highPrecedence) {
                    println("\nbad order: rewriting right in: ")
                    println("\ncur_op: " + cur_op)
                    println("\nleft: " + left)
                    println("\nright: " + right)
                    println("\n as:" + 
                      tokens.Expr.TmpInfixGroup(
                        op.unparent(),
                        tokens.Expr(tokens.Expr.TmpInfixGroup(
                          cur_op.unparent(),
                          left.unparent(),
                          expr1.unparent()
                        )),
                        expr2.unparent()))
                    splice(
                      tokens.Expr.TmpInfixGroup(
                        op.unparent(),
                        tokens.Expr(tokens.Expr.TmpInfixGroup(
                          cur_op.unparent(),
                          left.unparent(),
                          expr1.unparent()
                        )),
                        expr2.unparent()))
                  } else if (cur_tok.lowPrecedence < child_tok.lowPrecedence) {
                    println("\ngood order: Checked Right")
                    splice(
                      tokens.Expr.TmpInfixGroup.Checked(
                        cur_op.unparent(),
                        left.unparent(),
                        tokens.Expr(tokens.Expr.TmpInfixGroup.Checked(
                          op.unparent(),
                          expr1.unparent(),
                          expr2.unparent()
                        ))))
                  } else if (cur_tok.isAssociative && child_tok.isAssociative) {
                    println("\nassoc found: Checked Right")
                    splice(
                      tokens.Expr.TmpInfixGroup.Checked(
                        cur_op.unparent(),
                        left.unparent(),
                        tokens.Expr(tokens.Expr.TmpInfixGroup.Checked(
                          op.unparent(),
                          expr1.unparent(),
                          expr2.unparent()
                        ))))
                  } else {
                    println("\nError")
                    splice(tokens.Expr.SomeKindOfError())
                  }
        | on(
            parent(tokens.Expr) *>
            field(tokens.Expr.TmpInfixGroup.withChildren(
              field(tok(defns.InfixOperator.instances*))
              ~ field(tokens.Expr)
              ~ field(tokens.Expr)
              ~ trailing
            ))
              ~ trailing
          ).rewrite: (op, left, right) =>
            println("\nchecking off :")
            println("\nOp: " + op)
            println("\nLeft: " + left)
            println("\nRight: " + right)
            println("\nAs: " +
              tokens.Expr.TmpInfixGroup.Checked(
                op.unparent(),
                left.unparent(),
                right.unparent()
              ))
            splice(
              tokens.Expr.TmpInfixGroup.Checked(
                op.unparent(),
                left.unparent(),
                right.unparent()
              ))
  end orderOperations

  val resolveOperations = passDef:
    wellformed := prevWellformed.makeDerived:
      tokens.Expr.removeCases(tokens.Expr.TmpInfixGroup.Checked)
    
    pass(once = false, strategy = pass.bottomUp)
      .rules:
        on(
            parent(tokens.Expr) *>
              field(tokens.Expr.TmpInfixGroup.Checked.withChildren(
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
  end resolveOperations

  val resolveAlphas = passDef:
    wellformed := prevWellformed.makeDerived:
      tokens.Expr.removeCases(tokens.Id)
    
    pass(once = false, strategy = pass.bottomUp)
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
  end resolveAlphas

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
