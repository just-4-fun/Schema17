package just4fun.core.schema.formatters


import just4fun.core.schema._
import com.fasterxml.jackson.core.{JsonFactory, JsonGenerator, JsonParser, JsonToken}
import com.fasterxml.jackson.core.JsonToken._
import scala.collection.mutable.ArrayBuffer
import java.io.StringWriter


trait JsonArrayFormatter extends Formatter {

	type MMap = collection.mutable.HashMap[String, Any]
	val factory = new JsonFactory


	override def stringToObj[T<:Schema[T]](data: String, schema: T) = toObject(data, schema)
	override def stringToArray[T: Manifest](data: String, elementType: PropType[T])  = toArray(data, elementType)
	override def stringFromObj[T<:Schema[T]](schema: T)(obj: T#OBJ) = fromObject(schema)(obj)
	override def stringFromArray[T: Manifest](array: Array[T]) = fromArray(array)

	/* FROM */

	def fromObject[T<:Schema[T]](schema: T)(obj: T#OBJ, g: JsonGenerator = null): String = if (obj != null) {
		generate(g) { gener =>
			gener.writeStartArray()
			obj.getValuesAll { (n, prop, v) => writeValue(v, gener) }
			gener.writeEndArray()
		}
	} else "[]"

	def fromArray(a: Array[_], g: JsonGenerator = null): String = if (a != null) {
		generate(g) { gener =>
			gener.writeStartArray()
			a.foreach { writeValue(_, gener) }
			gener.writeEndArray()
		}
	} else "[]"
	private def generate(g: JsonGenerator)(execCode: JsonGenerator => Unit): String = {
		val topLevel = g == null
		val gener = if (topLevel) factory.createGenerator(new StringWriter) else g
		execCode(gener)
		if (topLevel) {
			gener.close()
			gener.getOutputTarget.toString
		} else null
	}
	private def writeValue[T<:Schema[T], O<:T#OBJ: Manifest, U: Manifest](v: Any, gener: JsonGenerator) = v match {
		case null => gener.writeNull()
		case v: String => gener.writeString(v)
		case v: Long => gener.writeNumber(v)
		case v: Int => gener.writeNumber(v)
		case v: Double => gener.writeNumber(v)
		case v: Float => gener.writeNumber(v)
		case v: Short => gener.writeNumber(v)
		case v: Byte => gener.writeNumber(v)
		case v: Boolean => gener.writeBoolean(v)
		case v: O => fromObject(v.scheme)(v, gener)
		case v: Iterable[U] => fromArray(v.toArray, gener)
		case v: Array[Byte] => gener.writeBinary(v)
		case v: Array[_] => fromArray(v, gener)
		//		case v: Map[_, _] => gener.fromMap(v)// TODO can gen fromMap
		case _ => gener.writeString(v.toString)
	}


	/* TO */


	def toObject[T<:Schema[T]](json: String = null, schema: T, p: JsonParser = null): T#OBJ = {
		schema() setValuesIterating { onNext =>
			parse(json, p) { (token, parser, topLevel) =>
				var doNext = true
				token match {
					case VALUE_STRING => onNext(prop => parser.getText)
					case VALUE_NUMBER_INT => onNext(prop => parser.getValueAsLong)
					case VALUE_NUMBER_FLOAT => onNext(prop => parser.getValueAsDouble)
					case VALUE_NULL => onNext(prop => null)
					case VALUE_FALSE => onNext(prop => false)
					case VALUE_TRUE => onNext(prop => true)
					case START_OBJECT => onNext(prop => flatten(json, parser)) // TODO can parse toMap
					case START_ARRAY => onNext { prop => prop.valueType match {
						case typ: ObjectPropType[_] => toObject(json, typ.schema, parser)
						case typ: ArrayPropType[_] => toArray(json, typ.elementType, parser)
						case _ => flatten(json, parser)
					} }
					case END_ARRAY | END_OBJECT => doNext = topLevel
					case null => doNext = false
					case t => // FIELD_NAME | NOT_AVAILABLE | VALUE_EMBEDDED_OBJECT
				}
				doNext
			}
		}
	}
	def toArray[T](json: String = null, elementType: PropType[T], p: JsonParser = null): Array[T] = {
		val a = ArrayBuffer[T]()
		parse(json, p) { (token, parser, topLevel) =>
			var doNext = true
			token match {
				case VALUE_STRING => a += elementType.evaluate(parser.getText)
				case VALUE_NUMBER_INT => a += elementType.evaluate(parser.getValueAsLong)
				case VALUE_NUMBER_FLOAT => a += elementType.evaluate(parser.getValueAsDouble)
				case VALUE_NULL => a += elementType.evaluate(null)
				case VALUE_FALSE => a += elementType.evaluate(false)
				case VALUE_TRUE => a += elementType.evaluate(true)
				case START_OBJECT => a += elementType.evaluate(flatten(json, parser)) // TODO can parse toMap
				case START_ARRAY => a += elementType.evaluate {
					elementType match {
						case typ: ObjectPropType[_] => toObject(json, typ.schema, parser)
						case typ: ArrayPropType[_] => toArray(json, typ.elementType, parser)
						case _ => flatten(json, parser)
					}
				} // TODO test Array of Arrays of ET ???
				case END_ARRAY | END_OBJECT => doNext = topLevel
				case null => doNext = false
				case t => // FIELD_NAME | NOT_AVAILABLE | VALUE_EMBEDDED_OBJECT
			}
			doNext
		}
		a.toArray(elementType.mft)
	}
	private def parse(json: String, p: JsonParser)(execNext: (JsonToken, JsonParser, Boolean) => Boolean) = {
		val topLevel = p == null
		val parser = if (topLevel) factory.createParser(json) else p
		if (topLevel) parser.nextToken // skip first START_OBJECT | START_ARRAY
		next(parser.nextToken)
		if (topLevel) parser.close()
		//DEFs
		def next(token: JsonToken): Unit = if (execNext(token, parser, topLevel)) next(parser.nextToken)
	}
	private def flatten(json: String, p: JsonParser) = {
		val p0 = p.getCurrentLocation.getCharOffset - 1
		p.skipChildren()
		val p1 = p.getCurrentLocation.getCharOffset
		json.substring(p0.toInt, p1.toInt)
	}
}