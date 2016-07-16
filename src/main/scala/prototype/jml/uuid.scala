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

import java.util.concurrent.TimeUnit
import java.util.Date

/** Build custom UUIDs. */
object uuid {
  val UUID_VARIANT: Long = 0xE000
  val UUID_VERSION: Long = 0xA000
  val DEFAULT_CLOCK = 'p'.getNumericValue.toShort

  def apply(createdAt: Date, fields: Traversable[String]): String =
    apply(createdAt, fields.mkString).toString.replace("-", "")

  def apply(createdAt: Date, field: String): java.util.UUID = {
    require(createdAt != null, "date cannot be null")
    require(field != null && field.nonEmpty, "field must be non-empty")
    // We can use up to 48 bits for a user-specified field:
    val node = truncate48(hashCode64(field.getBytes("UTF-8")))
    apply(createdAt.getTime, TimeUnit.MILLISECONDS, DEFAULT_CLOCK, node)
  }

  def apply(timestamp: Long, unit: TimeUnit, clock: Short, node: Long): java.util.UUID = {
    val ts:  Long = (unit.toMicros(timestamp) * 10) + 0x01b21dd213814000L
    val msb: Long = UUID_VERSION | (ts & 0xFFFFFFFFFFFFF000L) << 4 | (ts & 0x0FFF)
    val lsb: Long = ((UUID_VARIANT | clock) << 48) | (0xFFFFFFFFFFFFL & node)
    new java.util.UUID(msb, lsb)
  }

  /** Effective Java, 2nd Edition, p. 45,
    * equivalent to Java's hashCode for String since 1.2 except for initial value */
  def hashCode64(value: Array[Byte]): Long = {
    var result = 17
    for (byte <- value) {
      result = 31 * result + byte
    }
    result
  }

  /** Truncate to a 48 bit value, mixing in the dropped bits. */
  def truncate48(value: Long): Long = {
    (0xFFFFFFFFFFFFL & value) - (value >>> 16)
  }
}
