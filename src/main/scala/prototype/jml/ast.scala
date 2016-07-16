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

object ast {
  sealed trait Node

  // TODO: Richer Key, Value types?
  type Key = String
  case class Path(keys: Seq[Key]) extends Node
  case class Func(name: String, args: Seq[RExpr]) extends Node

  trait RExpr extends Node

  case class Field(field: Path, calls: Seq[Func]) extends RExpr
  case class RValue(value: Value, calls: Seq[Func]) extends RExpr

  trait Stmt extends Node
  case class Assignment(lvalue: Path, rexpr: RExpr) extends Stmt

  case class CaseExpr(value: Field, clauses: Seq[Clause]) extends Stmt

  case class Clause(pattern: Pattern, body: Seq[Stmt]) extends Node

  trait Pattern extends Node
  case class ValuePattern(value: Value) extends Pattern
  case object WildcardPattern extends Pattern

  case class Mapping(stmts: Seq[Stmt]) extends Node

  // Best not support full set of JSON values.
  trait Value extends Node
  case class TextValue(data: String) extends Value
  case class BooleanValue(data: Boolean) extends Value
  case class IntValue(data: BigInt) extends Value
  case class DecimalValue(data: BigDecimal) extends Value
}