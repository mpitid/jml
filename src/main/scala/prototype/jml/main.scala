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

import parser.JMLParsers
import org.json4s._
import org.json4s.native.JsonMethods._

object main {

  private val logger = org.log4s.getLogger
  implicit val formats = DefaultFormats

  val Input = "input"
  val Env = "env"

  def main(args: Array[String]) {
    val mapping :: (files @ (_ :: _)) = args.toList
    val p = new JMLParsers
    val tree = p.parse(io.Source.fromFile(mapping).mkString)
    logger.info(s"AST $tree")
    val program = generator(tree.asInstanceOf[ast.Mapping])
    logger.info(s"GEN $program")
    val environment = JObject(
      "now" -> JInt(System.currentTimeMillis())
    , "post" -> JObject("id" -> JString("42"))
    )
    files.foreach { filename =>
      val data = parse(io.Source.fromFile(filename).mkString).extract[JObject]
      // The mapping program has access to the original object under
      // `input`, and a context-specific environment under `env`
      val context = JObject(Input -> data, Env -> environment)
      println(pretty(render(program(context).map {
        case JObject(fields) =>
          // Make sure we filter out any objects we introduced
          JObject(fields.filterNot(x => Set(Input, Env).contains(x._1)))
        case x => x})))
    }
  }
}
