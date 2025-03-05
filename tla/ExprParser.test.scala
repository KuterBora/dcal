package distcompiler.tla

import distcompiler.*
import Builtin.{Error, SourceMarker}

// scala-cli test . -- '*ExprParser*'

class ExprParserTests extends munit.FunSuite:
  extension (str: String)
    def parseStr: Node.Top =
      val wrapped_str =
        Source.fromString(
          s"""---- MODULE TestMod ----
            |EXTENDS Naturals
            |VARIABLE temp
            |
            |Init == $str
            |====
          """.stripMargin
        )
      val top = TLAReader(SourceRange.entire(wrapped_str))
      TLAParser(top)
      ExprParser(top)
      Node.Top(
        top(tokens.Module)(tokens.Module.Defns)(tokens.Operator)(
          tokens.Expr
        ).unparentedChildren
      )

  extension (top: Node.Top)
    def parseNode: Node.Top =
      val freshTop = Node.Top(
        tokens.Module(
          tokens.Id("TestMod"),
          tokens.Module.Extends(),
          tokens.Module.Defns(
            tokens.Operator(
              tokens.Id("test"),
              tokens.Operator.Params(),
              tokens.Expr(
                top.unparentedChildren
              )))))
      ExprParser(freshTop)
      Node.Top(
        freshTop(tokens.Module)(tokens.Module.Defns)(tokens.Operator)(
          tokens.Expr
        ).unparentedChildren
      )

  test("NumberLiteral"):
    assertEquals("1".parseStr, Node.Top(tokens.Expr.NumberLiteral("1")))

  test("StringLiteral"):
    assertEquals(
      "\"string\"".parseStr,
      Node.Top(tokens.Expr.StringLiteral("string"))
    )
    assertEquals(
      "\"string\\nnewline\"".parseStr,
      Node.Top(tokens.Expr.StringLiteral("string\nnewline"))
    )

  test("Set Literal"):
    assertEquals(
      "{1, 2, 3}".parseStr,
      Node.Top(
        tokens.Expr.SetLiteral(
          tokens.Expr(tokens.Expr.NumberLiteral("1")),
          tokens.Expr(tokens.Expr.NumberLiteral("2")),
          tokens.Expr(tokens.Expr.NumberLiteral("3"))
        )))
    assertEquals(
      "{}".parseStr,
      Node.Top(tokens.Expr.SetLiteral())
    )
  
  test("TupleLiteral"):
    assertEquals(
      "<<1, 2, 3>>".parseStr,
      Node.Top(
        tokens.Expr.TupleLiteral(
          tokens.Expr(tokens.Expr.NumberLiteral("1")),
          tokens.Expr(tokens.Expr.NumberLiteral("2")),
          tokens.Expr(tokens.Expr.NumberLiteral("3"))
        )))
    assertEquals(
      "<<>>".parseStr,
      Node.Top(
        tokens.Expr.TupleLiteral()
      ))

  test("RecordLiteral"):
    assertEquals(
      "[x |-> 2, y |-> 3]".parseStr,
      Node.Top(
        tokens.Expr.RecordLiteral(
          tokens.Expr.RecordLiteral.Field(
            tokens.Id("x"),
            tokens.Expr(tokens.Expr.NumberLiteral("2"))
          ),
          tokens.Expr.RecordLiteral.Field(
            tokens.Id("y"),
            tokens.Expr(tokens.Expr.NumberLiteral("3"))
          ))))
  
  test("Projection (Record Field Acess)"):
    assertEquals(
      "x.y".parseStr,
      Node.Top(
        tokens.Expr.Project(
          tokens.Expr(
            tokens.Expr.OpCall(
              tokens.Id("x"),
              tokens.Expr.OpCall.Params()
            )),
          tokens.Id("y"))))
    assertEquals(
      "x.y.z".parseStr,
      Node.Top(
        tokens.Expr.Project(
          tokens.Expr(tokens.Expr.Project(
            tokens.Expr(
              tokens.Expr.OpCall(
                tokens.Id("x"),
                tokens.Expr.OpCall.Params()
              )),
            tokens.Id("y"))),
          tokens.Id("z"))))
    assertEquals(
      "[y |-> 2].y".parseStr,
      Node.Top(
        tokens.Expr.Project(
          tokens.Expr(
            tokens.Expr.RecordLiteral(
              tokens.Expr.RecordLiteral.Field(
                tokens.Id("y"),
                tokens.Expr(tokens.Expr.NumberLiteral("2"))
          )
            )),
          tokens.Id("y"))))

  test("RecordSetLiteral"):
    assertEquals(
      "[x : {1, 2}]".parseStr,
      Node.Top(
        tokens.Expr.RecordSetLiteral(
          tokens.Expr.RecordSetLiteral.Field(
            tokens.Id("x"),
            tokens.Expr(
              tokens.Expr.SetLiteral(
                tokens.Expr(tokens.Expr.NumberLiteral("1")),
                tokens.Expr(tokens.Expr.NumberLiteral("2"))
              ))))))

  test("Name"):
    assertEquals(
      "x".parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.Id("x"),
          tokens.Expr.OpCall.Params()
        )))

  // test("Single Binary Operator"):
  //   assertEquals(
  //     "5 + 6".parseStr,
  //     Node.Top(
  //       tokens.Expr.OpCall(
  //         tokens.OpSym(
  //           defns.+("+")
  //         ),
  //         tokens.Expr.OpCall.Params(
  //           tokens.Expr(tokens.Expr.NumberLiteral("5")),
  //           tokens.Expr(tokens.Expr.NumberLiteral("6"))
  //         ))))
  //   assertEquals(
  //     "5 $ x".parseStr,
  //     Node.Top(
  //       tokens.Expr.OpCall(
  //         tokens.OpSym(defns.$("$")),
  //         tokens.Expr.OpCall.Params(
  //           tokens.Expr(tokens.Expr.NumberLiteral("5")),
  //           tokens.Expr(
  //             tokens.Expr.OpCall(
  //               tokens.Id("x"),
  //               tokens.Expr.OpCall.Params()
  //             ))))))

  test("Precedence: Infix Operators"):
    assertEquals(
      "5 * 6 + 7".parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.OpSym(
            defns.+("+")
          ),
          tokens.Expr.OpCall.Params(
            tokens.Expr(
              tokens.Expr.OpCall(
                tokens.OpSym(
                  defns.*("*")
                ),
                tokens.Expr.OpCall.Params(
                  tokens.Expr(tokens.Expr.NumberLiteral("5")),
                  tokens.Expr(tokens.Expr.NumberLiteral("6"))
                ))),
                tokens.Expr(tokens.Expr.NumberLiteral("7"))))))
    // assertEquals(
    //   "1 + 8 * 6 - 9 * 3".parseStr,
    //   Node.Top())
      // Node.Top(
      //   tokens.Expr.OpCall(
      //     tokens.OpSym(
      //       defns.-("-")
      //     ),
      //     tokens.Expr.OpCall.Params(
      //       tokens.Expr.OpCall(
      //         tokens.OpSym(
      //           defns.+("+")
      //         ),
      //         tokens.Expr.OpCall.Params(
      //           tokens.Expr(tokens.Expr.NumberLiteral("1")),
      //           tokens.Expr(
      //             tokens.Expr.OpCall(
      //               tokens.OpSym(
      //                 defns.*("*")
      //               ),
      //               tokens.Expr.OpCall.Params(
      //                 tokens.Expr(tokens.Expr.NumberLiteral("8")),
      //                 tokens.Expr(tokens.Expr.NumberLiteral("6"))
      //               ))))),
      //       tokens.Expr(
      //         tokens.Expr.OpCall(
      //           tokens.OpSym(
      //             defns.*("*")
      //           ),
      //           tokens.Expr.OpCall.Params(
      //             tokens.Expr(tokens.Expr.NumberLiteral("9")),
      //             tokens.Expr(tokens.Expr.NumberLiteral("3"))
      //           )))))))
  // test("FnCall"): ???: what about x[a, b, c] - needs to be parsed to (Expr, Expr) 
  //   assertEquals(
  //     "x[\"y\"]".parseStr,
  //     Node.Top()
  //   )
  
  test("If"):
    assertEquals(
      """IF A 
       |THEN 1
       |ELSE 2""".stripMargin.parseStr,
      Node.Top(
        tokens.Expr.If(
          tokens.Expr(
            tokens.Expr.OpCall(
              tokens.Id("A"),
              tokens.Expr.OpCall.Params()
            )),
          tokens.Expr(tokens.Expr.NumberLiteral("1")),
          tokens.Expr(tokens.Expr.NumberLiteral("2"))
        )))
  
  test("Case"):
    assertEquals(
      """CASE A -> 1 
       | [] B -> 2""".stripMargin.parseStr,
      Node.Top(
        tokens.Expr.Case(
          tokens.Expr.Case.Branches(
            tokens.Expr.Case.Branch(
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("A"),
                tokens.Expr.OpCall.Params()
              )),
              tokens.Expr(tokens.Expr.NumberLiteral("1"))
            ),
            tokens.Expr.Case.Branch(
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("B"),
                tokens.Expr.OpCall.Params()
              )),
              tokens.Expr(tokens.Expr.NumberLiteral("2"))
            )),
          tokens.Expr.Case.Other(tokens.Expr.Case.Other.None())
        )))
    assertEquals(
      """CASE A -> 1 
       | [] B -> 2
       | OTHER -> 3""".stripMargin.parseStr,
      Node.Top(
        tokens.Expr.Case(
          tokens.Expr.Case.Branches(
            tokens.Expr.Case.Branch(
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("A"),
                tokens.Expr.OpCall.Params()
              )),
              tokens.Expr(tokens.Expr.NumberLiteral("1"))
            ),
            tokens.Expr.Case.Branch(
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("B"),
                tokens.Expr.OpCall.Params()
              )),
              tokens.Expr(tokens.Expr.NumberLiteral("2"))
            )),
          tokens.Expr.Case.Other(
            tokens.Expr(tokens.Expr.NumberLiteral("3"))
          ))))
  
  test("LET"):
    assertEquals(
      """LET x == 1
      |y == 2
      |IN  {y, x}""".stripMargin.parseStr,
      Node.Top(
        tokens.Expr.Let(
          tokens.Expr.Let.Defns(
            tokens.Operator(
              tokens.Id("x"),
              tokens.Operator.Params(),
              tokens.Expr(tokens.Expr.NumberLiteral("1"))
            ),
            tokens.Operator(
              tokens.Id("y"),
              tokens.Operator.Params(),
              tokens.Expr(tokens.Expr.NumberLiteral("2"))
            )
          ),
          tokens.Expr(
            tokens.Expr.SetLiteral(
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("y"),
                tokens.Expr.OpCall.Params()
              )),
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("x"),
                tokens.Expr.OpCall.Params()
              )))))))
  
  // test("Exists"):
  //   assertEquals(
  //     "\\E x \\in {1, 2, 3} : x = 2".parseStr,
  //     Node.Top()
  //  )
  
  test("ParenthesesGroup"):
    assertEquals(
      "(1)".parseStr,
      Node.Top(tokens.Expr.NumberLiteral("1"))
    )
    assertEquals(
      "(((x)))".parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.Id("x"),
          tokens.Expr.OpCall.Params()
        )))
    // assertEquals(
    //   "(5 + 6)".parseStr,
    //   Node.Top(
    //     tokens.Expr.OpCall(
    //       tokens.OpSym(
    //         defns.+("+")
    //       ),
    //       tokens.Expr.OpCall.Params(
    //         tokens.Expr(tokens.Expr.NumberLiteral("5")),
    //         tokens.Expr(tokens.Expr.NumberLiteral("6"))
    //       ))))
    // assertEquals(
    //   "(1 + 2) * 3".parseStr,
    //   Node.Top(
    //     tokens.Expr.OpCall(
    //       tokens.OpSym(
    //         defns.*("*")
    //       ),
    //       tokens.Expr.OpCall.Params(
    //         tokens.Expr(tokens.Expr.OpCall(
    //           tokens.OpSym(
    //             defns.+("+")
    //           ),
    //           tokens.Expr.OpCall.Params(
    //             tokens.Expr(tokens.Expr.NumberLiteral("1")),
    //             tokens.Expr(tokens.Expr.NumberLiteral("2"))
    //           )
    //         )),
    //         tokens.Expr(tokens.Expr.NumberLiteral("3"))
    //       ))))