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

import collection.mutable
import ast._


object generator {

  private val logger = org.log4s.getLogger

  class SymbolTable() {
    protected val values = mutable.HashMap[String, SymbolEntry]()

    def apply(key: String): SymbolEntry =
      values(key)

    def get(key: String): Option[SymbolEntry] =
      values get key

    def getVariable(key: String): Option[Variable] = get(key) match {
      case Some(v: Variable) => Some(v)
      case _ => None
    }

    def getFunction(key: String): Option[Function] = get(key) match {
      case Some(f: Function) => Some(f)
      case _ => None
    }

    def put(entry: SymbolEntry) = {
      val key = entry match {
        case Variable(k, _) => k
        case Function(k, _, _, _) => k
      }
      values.put(key, entry)
    }
  }

  sealed trait SymbolEntry
  case class Variable(name: String, t: Type) extends SymbolEntry
  case class Function(name: String, arguments: Args, result: Type, code: library.R) extends SymbolEntry
  case class Args(args: Seq[Type], varargs: Option[Type])
  object Args { def apply(args: Type*): Args = Args(args, None) }

  sealed trait Type
  trait Type1 extends Type
  trait Type2 extends Type
  case object AnyType extends Type
  case object NothingType extends Type // ??

  trait ScalarType extends Type1
  case object DateType extends ScalarType
  case object StringType extends ScalarType
  case object IntType extends ScalarType
  case object DecimalType extends ScalarType
  case object BooleanType extends ScalarType

  trait ContainerType extends Type1
  case object ObjectType extends ContainerType
  case object ArrayType extends ContainerType
  case class DetailedObjectType(fields: Map[String, Type]) extends ContainerType
  case class DetailedArrayType(contains: Type) extends ContainerType

  case class OneOf(types: Seq[Type1]) extends Type2

  object symbols {
    import library._

    val functions = Seq(
      Function("uuid", Args(Seq(DateType), Some(StringType)), StringType, uuid)
    , Function("or", Args(AnyType, AnyType), AnyType, or)
    , Function("concat", Args(Seq(), Some(AnyType)), StringType, concat)
    , Function("fromUnixTs", Args(OneOf(Seq(IntType, StringType))), DateType, fromUnixTs)
    , Function("fromMicroTs", Args(DecimalType), DateType, fromMicroTs)
    , Function("fromMilliTs", Args(OneOf(Seq(IntType, StringType))), DateType, fromMilliTs)
    , Function("fromISO8601", Args(StringType), DateType, fromISO8601(false))
    , Function("fromISO8601MS", Args(StringType), DateType, fromISO8601(true))
    , Function("asISO8601", Args(DateType), StringType, asISO8601(false))
    , Function("asISO8601MS", Args(DateType), StringType, asISO8601(true))
    , Function("asText", Args(OneOf(Seq(IntType, DecimalType, BooleanType, StringType))), StringType, asText)
    , Function("asInt", Args(StringType), IntType, asInt)
    , Function("asDecimal", Args(StringType), DecimalType, asDecimal)
    , Function("asBoolean", Args(StringType), BooleanType, asBoolean)
    , Function("transpose", Args(ObjectType), ObjectType, transpose)
    , Function("copy", Args(ObjectType), ObjectType, copy)
    , Function("snakize", Args(ObjectType), ObjectType, snakize)
    , Function("camelize", Args(ObjectType), ObjectType, camelize)
    , Function("replace", Args(Seq(StringType, StringType, StringType), None), StringType, replace)
    , Function("replaceRegex", Args(Seq(StringType, StringType, StringType), None), StringType, replaceRegex)
    , Function("stripTags", Args(Seq(StringType), None), StringType, stripTags)
    , Function("basename", Args(StringType), StringType, basename)
    , Function("dirname", Args(StringType), StringType, dirname)
    , Function("merge", Args(Seq(), Some(ObjectType)), ObjectType, merge)
    , Function("without", Args(Seq(ObjectType), Some(StringType)), ObjectType, without)
    , Function("asRFC2822", Args(DateType), StringType, asRFC2822)
    , Function("fromRFC2822", Args(DateType), StringType, fromRFC2822)
    )

    val variables = Seq(
      Variable("interaction", DetailedObjectType(Map(
        "content" -> StringType
      , "created_at" -> StringType
      , "received_at" -> StringType
      , "type" -> StringType
      , "subtype" -> StringType
      , "sample" -> DecimalType
      , "author" -> DetailedObjectType(Map(
          "id" -> StringType
      ,   "username" -> StringType
      ,   "name" -> StringType
      ,   "avatar" -> StringType
      ,   "url" -> StringType
        ))
      )))
    )
  }

  object library {

    import java.util.Date
    import java.text.SimpleDateFormat
    import org.json4s._

    // TODO: Should probably use JML domain here instead of JSON,
    // to allow first class support of types such as Dates, and
    // make functions more portable. On the other hand, functions
    // that operate on non-scalar types would not be supported (e.g. transpose).

    type R = PartialFunction[List[JValue], JValue]

    def without: R = {
      case (obj: JObject) :: keys if keys.nonEmpty && keys.forall(_.isInstanceOf[JString]) =>
        keys.foldLeft(obj: JValue) {
          case (o, JString(k)) => o.removeField(_._1 == k)
          case (_, _) => throw new IllegalStateException("not happening")
        }
    }
    def merge: R = {
      case objects if objects.forall(_.isInstanceOf[JObject]) =>
        objects.reduce(_ merge _)
    }

    def or: R = {
      case (JNull | JNothing) :: (v2: JValue) :: Nil => v2
      case (v1: JValue) :: _ :: Nil => v1
    }

    def concat: R = {
      case lst =>
        JString(lst.map(valueToString).mkString)
    }

    def basename: R = {
      case JString(value) :: Nil => JString(new java.io.File(value).getName)
    }

    def dirname: R = {
      case JString(value) :: Nil => JString(new java.io.File(value).getParent)
    }

    def valueToString(v: JValue): String = v match {
      case JString(s) => s
      case JInt(i) => i.toString
      case JDecimal(d) => d.toString
      case JDouble(d) => d.toString
      case JBool(b) => b.toString
      case v => throw new IllegalArgumentException(s"Unsupported value type $v")
    }

    def uuid: R = {
      case JInt(ms) :: fields if fields.forall(_.isInstanceOf[JString]) =>
        JString(prototype.jml.uuid(new Date(ms.toLong), fields.map(_.asInstanceOf[JString].values)))
    }


    def fromUnixTs: R = {
      case JInt(i) :: Nil => JInt(1000L * i.toLong)
      case JString(s) :: Nil => JInt(1000L * s.toInt)
    }

    def fromMilliTs: R = {
      case JInt(i) :: Nil => JInt(i)
      case JString(s) :: Nil => JInt(s.toLong)
    }

    def fromMicroTs: R = {
      case JDouble(d) :: Nil =>
        fromMicroTs(JDecimal(d) :: Nil)
      case JDecimal(d) :: Nil =>
        val seconds = d.toBigInt()
        val micros = d.setScale(3) - BigDecimal(seconds)
        JInt(seconds.toInt.toLong * 1000L + micros.toString.split('.')(1).toInt)
    }

    def fromISO8601(millis: Boolean = false): R = {
      case JString(s) :: Nil =>
        val format = s"yyyy-MM-dd'T'HH:mm:ss${if (millis) ".SSS" else ""}X"
        JInt(new SimpleDateFormat(format).parse(s).getTime)
    }

    def asISO8601(millis: Boolean = false): R = {
      case JInt(d) :: Nil =>
        val format = s"yyyy-MM-dd'T'HH:mm:ss${if (millis) ".SSS" else ""}X"
        JString(new SimpleDateFormat(format).format(d))
    }

    def copy: R = {
      case (x: JObject) :: Nil => x
    }

    def snakize: R = {
      case (o: JObject) :: Nil => o.snakizeKeys
    }

    def camelize: R = {
      case (o: JObject) :: Nil => o.camelizeKeys
    }

    def replace: R = {
      case JString(text) :: JString(what) :: JString(replacement) :: Nil =>
        JString(text.replace(what, replacement))
    }

    def replaceRegex: R = {
      case JString(text) :: JString(what) :: JString(replacement) :: Nil =>
        JString(what.r.replaceAllIn(text, replacement))
    }

    def stripTags: R = {
      case JString(s) :: Nil =>
        JString(tagRegex.replaceAllIn(s, ""))
    }

    protected val tagRegex = """<[a-zA-Z/][^>]*>""".r

    def asText: R = {
      case JString(s) :: Nil => JString(s)
      case JInt(i) :: Nil => JString(i.toString())
      case JDecimal(d) :: Nil => JString(d.toString())
      case JBool(b) :: Nil => JString(b.toString)
    }

    def asInt: R = {
      case JString(s) :: Nil => JInt(BigInt(s))
    }

    def asDecimal: R = {
      case JString(s) :: Nil => JDecimal(BigDecimal(s))
    }

    def asBoolean: R = {
      case JString(s) :: Nil => JBool(s == "true")
    }

    def asRFC2822: R = {
      case JInt(ms) :: Nil =>
        JString(new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z").format(new Date(ms.toLong)))
    }

    def fromRFC2822: R = {
      case JString(s) :: Nil =>
        JInt(new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z").parse(s).getTime)

    }

    def transpose: R = {
      case (a: JArray) :: Nil =>
        arrayToObject(a)
    }

    def lookup(value: JValue, keys: String*): JValue =
      (value, keys.headOption) match {
        case (JObject(fields), Some(key)) =>
          fields.collectFirst {
            case (k, v) if k == key =>
              lookup(v, keys.tail: _*)
          } getOrElse JNull
        case _ => JNull
      }

    // Convert an array of objects to an object of arrays
    def arrayToObject(arr: JArray): JObject = {
      val keys = arr.arr.flatMap {
        case JObject(fields) => fields.unzip._1
        case v => throw new IllegalArgumentException(s"Expected object instead of $v")
      }.toSet
      JObject(keys.toList.flatMap { k =>
        arr.arr.map {
          case JObject(fields) =>
            (k, fields.find(_._1 == k).map(_._2) getOrElse JNull)
          case other =>
            throw new IllegalArgumentException(s"unexpected value $other")
        }
      }.groupBy(_._1).map {
        case (k, vs) => JField(k, JArray(vs.unzip._2))
      }.toList)
    }

    def transpose[K, V](items: List[Map[K, V]]): Map[K, List[Option[V]]] = {
      val keys = items.flatMap(_.keys).toSet
      for (key <- keys) yield (key, items.map(_.get(key)))
    }.toMap

  }

  def apply(ast: Mapping) = {
    val table = new SymbolTable
    symbols.variables foreach table.put
    symbols.functions foreach table.put
    generate(ast, table)
  }

  import org.json4s._
  // Return a function that performs the transformation
  def generate(ast: Node, table: SymbolTable): (JObject) => JObject = ast match {
    case Mapping(stmts) =>
      handleStmts(stmts, table)
    case Assignment(lvalue, rexpr) =>
      logger.debug(s"Assigning $lvalue to $rexpr")
      val result = handleExpr(rexpr, table)
      (obj) =>
        assignValue(obj, lvalue.keys.toList, result(obj))
        //addValue(obj, lvalue.keys.toList, result(obj))
    case CaseExpr(value, clauses) =>
      logger.debug(s"Case expr on $value")
      val v = handleExpr(value, table)
      val cs = clauses.map(handleClause(_, table))
      (obj) =>
        val result = v(obj)
        val body = cs.collectFirst {
          case (matcher, body) if matcher(result) => body
        } getOrElse(throw new IllegalArgumentException("Match error"))
        body(obj)
    case e =>
      throw new IllegalStateException(s"Unexpected expr $e")
  }

  def handleStmts(stmts: Seq[Stmt], table: SymbolTable) =
    stmts.tail.foldLeft(generate(stmts.head, table)) {
      case (f, s) => f andThen generate(s, table)
    }

  def handleClause(clause: Clause, table: SymbolTable): (JValue => Boolean, (JObject) => JObject) = clause match {
    case Clause(WildcardPattern, body) =>
      ((_) => true, handleStmts(body, table))
    case Clause(ValuePattern(v), body) =>
      val jv = valueToJValue(v)
      ((o) => jv == o, handleStmts(body, table))
  }

  def handleExpr(ast: RExpr, table: SymbolTable): (JObject) => JValue = ast match {
    case RValue(v, fs) =>
      logger.debug(s"rvalue $v $fs")
      handleFunctionCalls((_) => valueToJValue(v), fs, table)
    case Field(path, calls) =>
      logger.debug(s"function field ${path.keys}")
      handleFunctionCalls((obj) => findValue(obj, path.keys), calls, table)
  }

  def functionMatches(function: Function, call: Func) = {
    function.arguments match {
      case Args(as, Some(va)) =>
        1 + call.args.size >= as.size
      case Args(as, None) =>
        1 + call.args.size == as.size
    }
  }

  def handleFunctionCalls(arg0: (JObject) => JValue, calls: Seq[Func], table: SymbolTable): (JObject) => JValue = {
    val funArgs = calls.map {
      case call @ Func(name, args) =>
        val fun = table.getFunction(name) getOrElse {
          throw new IllegalArgumentException(s"Undefined function $name")
        }
        if (!functionMatches(fun, call))
          throw new IllegalArgumentException(s"Arity mismatch for function $name")
        // Compile arguments
        val compiledArgs = args.map(handleExpr(_, table))
        (name, fun.code, compiledArgs)
    }
    obj =>
      funArgs.foldLeft(arg0(obj)) {
        case (v, (name, f, args)) =>
          logger.debug(s"Calling function $name")
          f(v :: args.map(_(obj)).toList)
      }
  }

  def findValue(obj: JObject, keys: Seq[String]): JValue = {
    logger.debug(s"Looking for ${keys.mkString(".")} in $obj")
    keys.foldLeft(obj: JValue) {
      case (JObject(fields), key) =>
        fields.collectFirst {
          case (k, v) if k == key => v
        } getOrElse JNothing
      case (v, key) =>
        JNothing
        //throw new IllegalArgumentException(s"Tried to access field $key of non-object value $v")
    }
  }

  def assignValue(obj: JObject, path: List[String], value: JValue): JObject = {
    require(path.nonEmpty, "where should this be assigned?")
    (obj, path) match {
      case (JObject(fields), key :: Nil) =>
        // innermost assignment, replace key in object with value
        JObject(key -> value :: fields.filterNot(_._1 == key))
      case (JObject(fields), key :: keys) =>
        // find key and apply recursively, create an object if key missing
        val sub = fields.collectFirst {
          case (k, o: JObject) if k == key => o
          case (k, o) if k == key =>
            throw new IllegalArgumentException(s"Cannot assign to field of $k of non-object $o")
        }.getOrElse { JObject() }
        JObject(key -> assignValue(sub, keys, value) :: fields.filterNot(_._1 == key))
    }
  }

  def addValue(obj: JObject, keys: List[String], value: JValue): JObject =
    (obj, keys) match {
      case (JObject(_), Nil) =>
        throw new AssertionError("addValue called with no keys")
      case (JObject(fields), key :: Nil) =>
        logger.debug(s"addValue $key $fields")
        JObject(key -> value :: fields.filterNot(_._1 == key))
      case (JObject(fields), key :: keys) =>
        logger.debug(s"addValue $keys $fields")
        JObject(key -> fields.collectFirst {
          case (k, o: JObject) if k == key =>
            addValue(o, keys, value)
          case (k, v) if k == key =>
            throw new IllegalArgumentException(s"Tried to access field $k of non-object value $v")
        }.getOrElse {
          addValue(JObject(), keys, value)
          //throw new IllegalArgumentException(s"Could not find $key in $obj")
        } :: fields.filterNot(_._1 == key))
  }

  def valueToJValue(value: Value): JValue = value match {
    case DecimalValue(v) => JDecimal(v)
    case IntValue(v) => JInt(v)
    case TextValue(v) => JString(v)
    case BooleanValue(v) => JBool(v)
  }
}
