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
  
  test("TupleLiteral"):
    assertEquals(
      "<<1, 2, 3>>".parseStr,
      Node.Top(
        tokens.Expr.TupleLiteral(
          tokens.Expr(tokens.Expr.NumberLiteral("1")),
          tokens.Expr(tokens.Expr.NumberLiteral("2")),
          tokens.Expr(tokens.Expr.NumberLiteral("3"))
        )))

  test("RecordLiteral"):
    assertEquals(
      "[x |-> 2, y |-> 3]".parseStr,
      Node.Top(
        tokens.Expr.RecordLiteral(
          tokens.Expr.RecordLiteral.Field(
            // tokens.Expr.OpCall(
            //   tokens.Id("x"),
            //   tokens.Expr.OpCall.Params()
            // ),
            tokens.Id("x"),
            tokens.Expr(tokens.Expr.NumberLiteral("2"))
          ),
          tokens.Expr.RecordLiteral.Field(
            // tokens.Expr.OpCall(
            //   tokens.Id("y"),
            //   tokens.Expr.OpCall.Params()
            // ),
            tokens.Id("y"),
            tokens.Expr(tokens.Expr.NumberLiteral("3"))
          ),
        )
      )
    )
  
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
      "5 $ 6".parseStr,
      Node.Top(
        tokens.Expr.OpCall(
          tokens.OpSym(defns.$("$")),
          tokens.Expr.OpCall.Params(
            tokens.Expr(tokens.Expr.NumberLiteral("5")),
            tokens.Expr(tokens.Expr.NumberLiteral("6"))
          ))))
  
  test("ParenthesesGroup"):
    assertEquals(
      "(1)".parseStr,
      Node.Top(tokens.Expr.NumberLiteral("1"))
    )
    assertEquals(
      "(((1)))".parseStr,
      Node.Top(tokens.Expr.NumberLiteral("1"))
    )
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