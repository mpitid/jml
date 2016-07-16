// The MIT License (MIT)
//
// Copyright (c) 2014 Michael Pitidis
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package prototype.jml

import util.parsing.combinator.JavaTokenParsers
import ast._
import util.parsing.input.CharSequenceReader


object parser {

  class JMLParsers extends JavaTokenParsers {

    // TODO: This makes # an invalid character everywhere else.
    // Need to switch to TokenParsers for proper comments.
    override protected val whiteSpace = """(\s|#.*)+""".r

    // Entry point
    def mapping: Parser[Mapping] =
      stmt.+ ^^ { Mapping }

    def path: Parser[Path] =
      key ~ ( "." ~> key ).* ^^ { case k ~ keys => Path(k :: keys) }

    def key: Parser[Key] =
      """[a-zA-Z_]+[a-zA-Z0-9_-]*""".r | """'[^']+'""".r ^^ { s => s.substring(1, s.length-1) }

    def stmt: Parser[Stmt] =
      caseExpr | path ~ ( "=" ~> rightExpr ) ^^ {
        case path ~ rexpr =>
          Assignment(path, rexpr)
      }

    def args: Parser[Seq[RExpr]] =
      ( rightExpr ~ ( "," ~> rightExpr ).* ).? ^^ {
        case Some(a1 ~ aN) => a1 :: aN
        case None => Nil
      }

    def func: Parser[Func] =
      name ~ ( "(" ~> args <~ ")" ) ^^ {
        case name ~ args =>
          Func(name, args)
      }

    def name: Parser[String] =
      """[a-zA-Z_]+[a-zA-Z0-9_]*""".r


    // Can't find less awkward way to support . for functions as well...
    def keyFun: Parser[Field] =
      ("." ~> func).+ ^^ { Field(Path(Nil), _) } | ("." ~> key ~ keyFun.?) ^^ {
        case k ~ r =>
          Field(Path(k :: r.map(_.field.keys.toList).getOrElse(Nil)), r.map(_.calls).getOrElse(Nil))
      }

    def rightExpr: Parser[RExpr] =
      rightValue | fieldExpr

    def rightValue: Parser[RValue] =
      value ~ ("." ~> func).* ^^ {
        case v ~ fs =>
          RValue(v, fs)
      }

    // TODO: Need to support values as well.
    def fieldExpr: Parser[Field] =
      key ~ keyFun.? ^^ {
        case k ~ r =>
          Field(Path(k :: r.map(_.field.keys.toList).getOrElse(Nil)), r.map(_.calls).getOrElse(Nil))
      }
//      path ~ ( ":" ~> func).* ^^ {
//        case path ~ funcs =>
//          RExpr(path, funcs)
//      }

    def caseExpr: Parser[CaseExpr] =
      "case" ~> fieldExpr ~ ( "of" ~> "{" ~> clause.+ <~ "}" ) ^^ {
        case rexpr ~ clauses =>
          CaseExpr(rexpr, clauses)
      }

    def clause: Parser[Clause] =
      pattern ~ ( "=>" ~> stmt.+ ) ^^ {
        case pattern ~ body =>
          Clause(pattern, body)
      }

    def pattern: Parser[Pattern] =
      "_" ^^^ { WildcardPattern } | value ^^ { ValuePattern }

    // TODO: First class support for containers?
    def value: Parser[Value] =
      booleanValue | textValue | decimalValue | intValue

    // TODO: This ignores all whitespace...
    def textValue: Parser[TextValue] =
      "\"" ~> validChar.* <~ "\"" ^^ { s => TextValue(s.mkString) }

    def validChar: Parser[String] =
      """[^"\\]""".r | """\\""" | "\\\""

    def booleanValue: Parser[BooleanValue] =
      ( "true" | "false" ) ^^ { v => BooleanValue(v == "true")}

    def intValue: Parser[IntValue] =
      "[0-9]+".r ^^ { v => IntValue(BigInt(v)) }

    def decimalValue: Parser[DecimalValue] =
      "[0-9]+\\.[0-9]+".r ^^ { v => DecimalValue(BigDecimal(v)) }

    def parse(data: String, parser: Parser[Node] = mapping) = {
      val input = new CharSequenceReader(data)
      phrase(parser)(input) match {
        case Success(ast, _) => ast
        case NoSuccess(msg, _) =>
          throw new IllegalArgumentException(msg)
      }
    }
  }
}
