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
  extension (str: String)
    def withoutParse: Node.Top =
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

  test("Single Binary Operator"):
    assertEquals(
      "5 + 6".parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.OpSym(
            defns.+("+")
          ),
          tokens.Expr.OpCall.Params(
            tokens.Expr(tokens.Expr.NumberLiteral("5")),
            tokens.Expr(tokens.Expr.NumberLiteral("6"))
          ))))
    assertEquals(
      "5 $ x".parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.OpSym(defns.$("$")),
          tokens.Expr.OpCall.Params(
            tokens.Expr(tokens.Expr.NumberLiteral("5")),
            tokens.Expr(
              tokens.Expr.OpCall(
                tokens.Id("x"),
                tokens.Expr.OpCall.Params()
              ))))))
    
  test("Single Unary Operator"):
    assertEquals(
      "UNION A".parseStr,
      Node.Top(tokens.Expr.OpCall(
        tokens.OpSym(defns.UNION("UNION")),
        tokens.Expr.OpCall.Params(
          tokens.Expr(
            tokens.Expr.OpCall(
              tokens.Id("A"),
              tokens.Expr.OpCall.Params()
            ))))))
    assertEquals(
      "x'".parseStr,
      Node.Top(tokens.Expr.OpCall(
        tokens.OpSym(defns.`'`("'")),
        tokens.Expr.OpCall.Params(
          tokens.Expr(
            tokens.Expr.OpCall(
              tokens.Id("x"),
              tokens.Expr.OpCall.Params()
            ))))))

  test("Precedence: Infix - Infix"):
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
    assertEquals(
      "1 + 8 * 6 - 9 * 3".parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.OpSym(defns.+("+")),
          tokens.Expr.OpCall.Params(
            tokens.Expr(tokens.Expr.NumberLiteral("1")),
            tokens.Expr(tokens.Expr.OpCall(
              tokens.OpSym(defns.-("-")),
              tokens.Expr.OpCall.Params(
                tokens.Expr(tokens.Expr.OpCall(
                  tokens.OpSym(defns.*("*")),
                  tokens.Expr.OpCall.Params(
                    tokens.Expr(tokens.Expr.NumberLiteral("8")),
                    tokens.Expr(tokens.Expr.NumberLiteral("6"))
                  ))),
                tokens.Expr(tokens.Expr.OpCall(
                  tokens.OpSym(defns.*("*")),
                  tokens.Expr.OpCall.Params(
                    tokens.Expr(tokens.Expr.NumberLiteral("9")),
                    tokens.Expr(tokens.Expr.NumberLiteral("3"))
                  ))))))))))

  test("Precedence: Infix - Prefix"):
    assertEquals(
      "~x /\\ y".parseStr,
      Node.Top(tokens.Expr.OpCall(
        tokens.OpSym(defns./\("/\\")),
        tokens.Expr.OpCall.Params(
          tokens.Expr(tokens.Expr.OpCall(
            tokens.OpSym(defns.~("~")),
            tokens.Expr.OpCall.Params(
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("x"),
                tokens.Expr.OpCall.Params()
              ))))),
          tokens.Expr(tokens.Expr.OpCall(
            tokens.Id("y"),
            tokens.Expr.OpCall.Params()
          ))))))
    assertEquals(
      "~x \\in y".parseStr,
      Node.Top(tokens.Expr.OpCall(
        tokens.OpSym(defns.~("~")),
        tokens.Expr.OpCall.Params(
          tokens.Expr(tokens.Expr.OpCall(
            tokens.OpSym(defns.`\\in`("\\in")),
            tokens.Expr.OpCall.Params(
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("x"),
                tokens.Expr.OpCall.Params()
              )),
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("y"),
                tokens.Expr.OpCall.Params()
              )))))))))
  
  test("Precedence: Infix - Postfix"):
    assertEquals(
      "1 + x' * 2".parseStr,
      Node.Top(tokens.Expr.OpCall(
        tokens.OpSym(defns.+("+")),
        tokens.Expr.OpCall.Params(
          tokens.Expr(tokens.Expr.NumberLiteral("1")),
          tokens.Expr(tokens.Expr.OpCall(
            tokens.OpSym(defns.*("*")),
            tokens.Expr.OpCall.Params(
              tokens.Expr(tokens.Expr.OpCall(
                tokens.OpSym(defns.`'`("'")),
                tokens.Expr.OpCall.Params(
                  tokens.Expr(tokens.Expr.OpCall(
                    tokens.Id("x"),
                    tokens.Expr.OpCall.Params()
                  ))))),
              tokens.Expr(tokens.Expr.NumberLiteral("2"))
            )))))))
  
  test("Precedence: Unary - Unary"):
    assertEquals(
      "[] ~x".parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.OpSym(defns.`[]`("[]")),
          tokens.Expr.OpCall.Params(
            tokens.Expr(tokens.Expr.OpCall(
              tokens.OpSym(defns.~("~")),
              tokens.Expr.OpCall.Params(
                tokens.Expr(tokens.Expr.OpCall(
                  tokens.Id("x"),
                  tokens.Expr.OpCall.Params()
                )))))))))  
  
      // TODO???: postfix postfix: never allowed
  
  test("Precedence: Associative"):
    assertEquals(
      "1 + 2 + 3".parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.OpSym(defns.+("+")),
          tokens.Expr.OpCall.Params(
            tokens.Expr(tokens.Expr.NumberLiteral("1")),
            tokens.Expr(tokens.Expr.OpCall(
              tokens.OpSym(defns.+("+")),
              tokens.Expr.OpCall.Params(
                tokens.Expr(tokens.Expr.NumberLiteral("2")),
                tokens.Expr(tokens.Expr.NumberLiteral("3"))
              )))))))
    assertEquals(
      "1 * 2 / 3".parseStr,
      Node.Top(tokens.Expr(
        Node(Builtin.Error)(
          Builtin.Error.Message(
            s"distcompiler.tla.defns.* and distcompiler.tla.defns./ must have different precedence, or be duplicates of an associative operator."
          ),
          Builtin.Error.AST(
            tokens.Expr.TmpInfixGroup(
              defns.*("*"),
              tokens.Expr(tokens.Expr.NumberLiteral("1")),
              tokens.Expr.TmpInfixGroup(
                defns./("/"),
                tokens.Expr(tokens.Expr.NumberLiteral("2")),
                tokens.Expr(tokens.Expr.NumberLiteral("3"))
              )))))))
  
  // Builtin Error should stop rewrite rules
  // TODO???: error message structure
  // TODO: depends on the location of resolveAlphas
  // test("Precedence: Error"):
  //   assertEquals(
  //     "x \\in A \\in B".parseStr,
  //     Node.Top(tokens.Expr(
  //       Node(Builtin.Error)(
  //         Builtin.Error.Message(
  //           s"26:distcompiler.tla.defns.\\in and 26:distcompiler.tla.defns.\\in must have different precedence, or be duplicates of an associative operator."
  //         ),
  //         Builtin.Error.AST(
  //           tokens.Expr.TmpInfixGroup(
  //             defns.`\\in`("\\in"),
  //             tokens.Expr(tokens.Id("x")),
  //             tokens.Expr.TmpInfixGroup(
  //               defns.`\\in`("\\in"),
  //               tokens.Expr(tokens.Id("A")),
  //               tokens.Expr(tokens.Id("B"))
  //             )))))))
  //   assertEquals(
  //     "x \\in A = B".parseStr,
  //     Node.Top(tokens.Expr(
  //       Node(Builtin.Error)(
  //         Builtin.Error.Message(
  //           s"26:distcompiler.tla.defns.\\in and distcompiler.tla.defns.= must have different precedence, or be duplicates of an associative operator."
  //         ),
  //         Builtin.Error.AST(
  //           tokens.Expr.TmpInfixGroup(
  //             defns.`\\in`("\\in"),
  //             tokens.Expr(tokens.Id("x")),
  //             tokens.Expr.TmpInfixGroup(
  //               defns.`=`("="),
  //               tokens.Expr(tokens.Id("A")),
  //               tokens.Expr(tokens.Id("B"))
  //             )))))))

  test("OpCall"):
    assertEquals(
      "testFun(1, 2, 3)".parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.Id("testFun"),
          tokens.Expr.OpCall.Params(
            tokens.Expr(tokens.Expr.NumberLiteral("1")),
            tokens.Expr(tokens.Expr.NumberLiteral("2")),
            tokens.Expr(tokens.Expr.NumberLiteral("3"))
          ))))

  // TODO: conjunction alignment

  test("FnCall"):
    assertEquals(
      "x[\"y\"]".parseStr,
      Node.Top(
        tokens.Expr.FnCall(
          tokens.Expr(tokens.Expr.OpCall(
            tokens.Id("x"),
            tokens.Expr.OpCall.Params()
          )),
          tokens.Expr(tokens.Expr.StringLiteral("y"))
        )))
    assertEquals(
      "x[1, 2, 3]".parseStr,
      Node.Top(
        tokens.Expr.FnCall(
          tokens.Expr(tokens.Expr.OpCall(
            tokens.Id("x"),
            tokens.Expr.OpCall.Params()
          )),
          tokens.Expr(tokens.Expr.TupleLiteral(
            tokens.Expr(tokens.Expr.NumberLiteral("1")),
            tokens.Expr(tokens.Expr.NumberLiteral("2")),
            tokens.Expr(tokens.Expr.NumberLiteral("3")),
          ))
        )))
  
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
  
  test("Exists"):
    assertEquals(
      "\\E x \\in {1, 2, 3} : x = 2".parseStr,
      Node.Top(
        tokens.Expr.Exists(
          tokens.QuantifierBounds(
            tokens.QuantifierBound(
              tokens.Id("x"),
              tokens.Expr(tokens.Expr.SetLiteral(
                tokens.Expr(tokens.Expr.NumberLiteral("1")),
                tokens.Expr(tokens.Expr.NumberLiteral("2")),
                tokens.Expr(tokens.Expr.NumberLiteral("3"))
              )))),
          tokens.Expr(tokens.Expr.OpCall(
            tokens.OpSym(defns.`=`("=")),
            tokens.Expr.OpCall.Params(
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("x"),
                tokens.Expr.OpCall.Params()
              )),
              tokens.Expr(tokens.Expr.NumberLiteral("2"))
            ))))))
    assertEquals(
      "\\E <<x, y, z>> \\in {1, 2, 3} : x = 2".parseStr,
      Node.Top(
        tokens.Expr.Exists(
          tokens.QuantifierBounds(
            tokens.QuantifierBound(
              tokens.Ids(
                tokens.Id("x"),
                tokens.Id("y"),
                tokens.Id("z")
              ),
              tokens.Expr(tokens.Expr.SetLiteral(
                tokens.Expr(tokens.Expr.NumberLiteral("1")),
                tokens.Expr(tokens.Expr.NumberLiteral("2")),
                tokens.Expr(tokens.Expr.NumberLiteral("3"))
              )))),
          tokens.Expr(tokens.Expr.OpCall(
            tokens.OpSym(defns.`=`("=")),
            tokens.Expr.OpCall.Params(
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("x"),
                tokens.Expr.OpCall.Params()
              )),
              tokens.Expr(tokens.Expr.NumberLiteral("2"))
            ))))))
    assertEquals(
      "\\E x \\in {1, 2, 3}, y \\in {4, 5, 6} : x = 2".parseStr,
      Node.Top(
        tokens.Expr.Exists(
          tokens.QuantifierBounds(
            tokens.QuantifierBound(
              tokens.Id("x"),
              tokens.Expr(tokens.Expr.SetLiteral(
                tokens.Expr(tokens.Expr.NumberLiteral("1")),
                tokens.Expr(tokens.Expr.NumberLiteral("2")),
                tokens.Expr(tokens.Expr.NumberLiteral("3"))
              ))),
            tokens.QuantifierBound(
              tokens.Id("y"),
              tokens.Expr(tokens.Expr.SetLiteral(
                tokens.Expr(tokens.Expr.NumberLiteral("4")),
                tokens.Expr(tokens.Expr.NumberLiteral("5")),
                tokens.Expr(tokens.Expr.NumberLiteral("6"))
              )))),
          tokens.Expr(tokens.Expr.OpCall(
            tokens.OpSym(defns.`=`("=")),
            tokens.Expr.OpCall.Params(
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("x"),
                tokens.Expr.OpCall.Params()
              )),
              tokens.Expr(tokens.Expr.NumberLiteral("2"))
            ))))))

  test("Forall"):
    assertEquals(
      "\\A x \\in {1, 2, 3} : x = 2".parseStr,
      Node.Top(
        tokens.Expr.Forall(
          tokens.QuantifierBounds(
            tokens.QuantifierBound(
              tokens.Id("x"),
              tokens.Expr(tokens.Expr.SetLiteral(
                tokens.Expr(tokens.Expr.NumberLiteral("1")),
                tokens.Expr(tokens.Expr.NumberLiteral("2")),
                tokens.Expr(tokens.Expr.NumberLiteral("3"))
              )))),
          tokens.Expr(tokens.Expr.OpCall(
            tokens.OpSym(defns.`=`("=")),
            tokens.Expr.OpCall.Params(
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("x"),
                tokens.Expr.OpCall.Params()
              )),
              tokens.Expr(tokens.Expr.NumberLiteral("2"))
            ))))))
  
  // TODO: AA EE ??
  
  test("Temporal Logic Combined"):
    assertEquals(
      "s /\\ \\E x \\in y : z /\\ \\E p \\in q : r".parseStr, //TODO???: confirm which one should come first?
      Node.Top(
        tokens.Expr.OpCall(
          tokens.OpSym(defns./\("/\\")),
          tokens.Expr.OpCall.Params(
            tokens.Expr(tokens.Expr.OpCall(
              tokens.Id("s"),
              tokens.Expr.OpCall.Params()
            )),
            tokens.Expr(tokens.Expr.Exists(
              tokens.QuantifierBounds(
                tokens.QuantifierBound(
                  tokens.Id("x"),
                  tokens.Expr(tokens.Expr.OpCall(
                    tokens.Id("y"),
                    tokens.Expr.OpCall.Params()
                  )))),
              tokens.Expr(tokens.Expr.OpCall(
                tokens.OpSym(defns./\("/\\")),
                tokens.Expr.OpCall.Params(
                  tokens.Expr(tokens.Expr.OpCall(
                    tokens.Id("z"),
                    tokens.Expr.OpCall.Params()
                  )),
                  tokens.Expr(tokens.Expr.Exists(
                    tokens.QuantifierBounds(
                      tokens.QuantifierBound(
                        tokens.Id("p"),
                        tokens.Expr(tokens.Expr.OpCall(
                          tokens.Id("q"),
                          tokens.Expr.OpCall.Params()
                        )))),
                    tokens.Expr(tokens.Expr.OpCall(
                      tokens.Id("r"),
                      tokens.Expr.OpCall.Params()
                    )))))))))))))

  // todo: function
  // todo: set comprehension
  // todo: set refinement
  // todo: lamda ??? Order2, grammar just says Expr
  
  test("Choose"):
    assertEquals(
      "CHOOSE x \\in {1, 2, 3} : x = 2".parseStr,
      Node.Top(
        tokens.Expr.Choose(
          tokens.QuantifierBound(
            tokens.Id("x"),
            tokens.Expr(tokens.Expr.SetLiteral(
              tokens.Expr(tokens.Expr.NumberLiteral("1")),
              tokens.Expr(tokens.Expr.NumberLiteral("2")),
              tokens.Expr(tokens.Expr.NumberLiteral("3"))
            ))),
          tokens.Expr(tokens.Expr.OpCall(
            tokens.OpSym(defns.`=`("=")),
            tokens.Expr.OpCall.Params(
              tokens.Expr(tokens.Expr.OpCall(
                tokens.Id("x"),
                tokens.Expr.OpCall.Params()
              )),
              tokens.Expr(tokens.Expr.NumberLiteral("2"))
            ))))))
    // todo: id nil, tuple expr, tuple nil

  // todo: except
  
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
    assertEquals(
      "(5 + 6)".parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.OpSym(
            defns.+("+")
          ),
          tokens.Expr.OpCall.Params(
            tokens.Expr(tokens.Expr.NumberLiteral("5")),
            tokens.Expr(tokens.Expr.NumberLiteral("6"))
          ))))
    assertEquals(
      "(1 + 2) * 3".parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.OpSym(
            defns.*("*")
          ),
          tokens.Expr.OpCall.Params(
            tokens.Expr(tokens.Expr.OpCall(
              tokens.OpSym(
                defns.+("+")
              ),
              tokens.Expr.OpCall.Params(
                tokens.Expr(tokens.Expr.NumberLiteral("1")),
                tokens.Expr(tokens.Expr.NumberLiteral("2"))
              )
            )),
            tokens.Expr(tokens.Expr.NumberLiteral("3"))
          ))))

  test("Conjunctions"):
    assertEquals(
      "/\\ 1".parseStr,
      Node.Top(
        tokens.Expr.NumberLiteral("1")
      ))
    assertEquals(
      "/\\ 1 \\/ 2".parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.OpSym(defns.\/("\\/")),
          tokens.Expr.OpCall.Params(
            tokens.Expr(tokens.Expr.NumberLiteral("1")),
            tokens.Expr(tokens.Expr.NumberLiteral("2"))
          ))))
    assertEquals(
      s"""
      |/\\ 1
      |/\\ 2
      |""".stripMargin.parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.OpSym(defns./\("/\\")),
          tokens.Expr.OpCall.Params(
            tokens.Expr(tokens.Expr.NumberLiteral("1")),
            tokens.Expr(tokens.Expr.NumberLiteral("2"))
        ))))
    assertEquals(
      s"""
      |/\\ 1 \\/ 1
      |/\\ 2 \\/ 2
      |/\\ 3 \\/ 3
      |""".stripMargin.parseStr,
      Node.Top())


    // /\ 1 
    //   /\ 2 
    // /\ 3 

    // /\ 1 
    //   /\ 2
    //   /\ 3
    //  /\ 4 
    // /\ 5
    //  /\ 6

