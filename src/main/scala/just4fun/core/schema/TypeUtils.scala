package just4fun.core.schema

import scala.collection.mutable.ArrayBuffer


/* FORMATTER */

trait Formatter {
	/** Change default encoding by overriding if required. */
	val charset: String = "UTF-8"

	def stringToObj[T <: Schema[T]](data: String, schema: T): T#OBJ = if (data == null) null.asInstanceOf[T#OBJ] else schema()
	def stringToArray[T: Manifest](data: String, elementType: PropType[T]): Array[T] = if (data == null) null else Array[T]()
	def stringFromObj[T <: Schema[T]](schema: T)(obj: T#OBJ): String = if (obj == null) null else ""
	def stringFromArray[T: Manifest](array: Array[T]): String = if (array == null) null else ""
	// Same for Array[Byte]
	def bytesToObj[T <: Schema[T]](data: Array[Byte], schema: T): T#OBJ = stringToObj(if (data == null) null else new String(data, charset), schema)
	def bytesToArray[T: Manifest](data: Array[Byte], elementType: PropType[T]): Array[T] = stringToArray(if (data == null) null else new String(data, charset), elementType)
	def bytesFromObj[T <: Schema[T]](schema: T)(obj: T#OBJ): Array[Byte] = stringFromObj(schema)(obj).getBytes(charset)
	def bytesFromArray[T: Manifest](array: Array[T]): Array[Byte] = stringFromArray(array).getBytes(charset)
}

/* TYPE UTILS interface */

trait TypeUtils {this: Formatter =>

	case class any2object[T <: Schema[T]](schema: T)(implicit m: Manifest[T#OBJ]) extends (Any => T#OBJ) {
		override def apply(v: Any): T#OBJ = v match {
			case v: T#OBJ => v
			case null => null.asInstanceOf[T#OBJ]
			case v: collection.Map[_, _] => schema().setValuesMap(v.asInstanceOf[Map[String, Any]])
			case v: Array[Byte] => bytesToObj(v, schema)
			case v: Array[_] => schema().setValuesArray(v)
			case v: Iterable[_] => schema().setValuesArray(v)
			case v: String => stringToObj(v, schema)
			case _ => logCastError(s"${schema.theName }#Object", v); null.asInstanceOf[T#OBJ]
		}
	}

	case class any2array[T: Manifest](elementType: PropType[T]) extends (Any => Array[T]) {
		val eltType = elementType.mft.toString().toLowerCase
		override def apply(v: Any): Array[T] = v match {
			case null => null
			case v: Array[Byte] => bytesToArray(v, elementType)
			case v: Array[_] => toArray(detectArrayType(v), v)
			case v: Iterable[_] => val a = new ArrayBuffer[T]; v.foreach(a += elementType.evaluate(_)); a.toArray
			case v: String => stringToArray(v, elementType)
			case _ => logCastError("Array", v); null
		}
		def toArray(typ: String, src: Array[_]): Array[T] = {
			//			println(s"Same? ${eltType.endsWith(typ) };  Mft= $eltType;  Array [$typ]= ${src.mkString(",") }");
			if (eltType.endsWith(typ)) src.asInstanceOf[Array[T]]
			else {
				val res = new Array[T](src.length)
				for (n <- 0 until res.length) res(n) = elementType.evaluate(src(n))
				res
			}
		}
		def detectArrayType[S <: Schema[S]](arr: Any): String = arr match {
			case v: Array[String] => "string"
			case v: Array[Int] => "int"
			case v: Array[Double] => "double"
			case v: Array[Long] => "long"
			case v: Array[Float] => "float"
			case v: Array[Boolean] => "boolean"
			// !!! Dangerous: detects type by first element only
			case v: Array[Array[_]] => if (v.length > 0) s"array[${detectArrayType(v(0)) }]" else "array?"
			// !!! Dangerous: detects type by first element only
			case v: Array[S#OBJ] => if (v.length > 0) v(0).getClass.getName.toLowerCase else "obj?"
			//			case v: Array[Byte] => "byte"
			//			case v: Array[Short] => "short"
			//			case v: Array[Char] => "char"
			case _ => "?"
		}
	}

	def any2objectFn[T <: Schema[T]](schema: T)(implicit m: Manifest[T#OBJ]): Any => T#OBJ = {
		any2object(schema)
	}
	def any2arrayFn[T: Manifest](elementType: PropType[T]): Any => Array[T] = {
		any2array(elementType)
	}
	def any2string(v: Any): String = v match {
		case v: String => v
		case null => null
		case v: Array[Byte] => new String(v, charset)
		case _ => v.toString
	}
	def any2bytes(v: Any): Array[Byte] = v match {
		case v: Array[Byte] => v
		case null => null
		case v: String => v.getBytes(charset)
		case _ => v.toString.getBytes(charset)
	}
	def any2long(v: Any): Long = v match {
		case v: Long => v
		case null => 0L
		case v: Double => v.toLong
		case v: Int => v.toLong
		case v: Float => v.toLong
		case v: String => castNumber(v.toLong)(v, any2long)
		case v: Boolean => if (v) 1L else 0L
		case v: Short => v.toLong
		case v: Byte => v.toLong
		case v: Char => v.toLong
		case v: Array[Byte] => any2long(new String(v, charset))
		case _ => logCastError("Long", v); 0L
	}
	def any2int(v: Any): Int = v match {
		case v: Int => v
		case null => 0
		case v: Long => v.toInt
		case v: Double => v.toInt
		case v: Float => v.toInt
		case v: String => castNumber(v.toInt)(v, any2int)
		case v: Boolean => if (v) 1 else 0
		case v: Short => v.toInt
		case v: Byte => v.toInt
		case v: Char => v.toInt
		case v: Array[Byte] => any2int(new String(v, charset))
		case _ => logCastError("Int", v); 0
	}
	def any2double(v: Any): Double = v match {
		case v: Double => v
		case null => 0d
		case v: Long => v.toDouble
		case v: Int => v.toDouble
		case v: Float => v.toDouble
		case v: String => castNumber(v.toDouble)(v, any2double)
		case v: Boolean => if (v) 1d else 0d
		case v: Short => v.toDouble
		case v: Byte => v.toDouble
		case v: Char => v.toDouble
		case v: Array[Byte] => any2double(new String(v, charset))
		case _ => logCastError("Double", v); 0d
	}
	def any2float(v: Any): Float = v match {
		case v: Float => v
		case null => 0f
		case v: Double => v.toFloat
		case v: Long => v.toFloat
		case v: Int => v.toFloat
		case v: String => castNumber(v.toFloat)(v, any2float)
		case v: Boolean => if (v) 1f else 0f
		case v: Short => v.toFloat
		case v: Byte => v.toFloat
		case v: Char => v.toFloat
		case v: Array[Byte] => any2float(new String(v, charset))
		case _ => logCastError("Float", v); 0f
	}
	def any2boolean(v: Any): Boolean = v match {
		case v: Boolean => v
		case null => false
		case 0 => false
		case 1 => true
		case v: String => val s = v.toLowerCase
			s != "" && s != "false" && s != "0" && s != "null" && s != "0.0" && s != "0,0"
		case v: Array[Byte] => any2boolean(new String(v, charset))
		case _ => true
	}


	def castNumber[N](castCode: => N)(v: String, failover: Any => N): N = try {
		castCode
	} catch {case _: NumberFormatException => failover(string2double(v)) }

	val NumPattern = """[\D&&[^\.,\-]]*(\-?[\D&&[^\.,]]*)(\d*)([\.,]*)(\d*).*""".r
	def string2double(v: String): Double = try {
		var NumPattern(sig, r, pt, f) = v
		//		println(s"($sig)($r)($pt)($f)")
		var mult = if (sig.endsWith("-")) -1 else 1
		if (r.length == 0) r = "0"
		if (f.length == 0) f = "0"
		if (pt.length > 1) {if (r != "0") f = "0" else mult = 1 }
		s"$r.$f".toDouble * mult
	} catch {case e: Throwable => logCastError("Number", v); 0d }

	def isPrimitive[T: Manifest](v: T) = manifest[T] <:< manifest[AnyVal]

	def logCastError(typ: String, v: Any) { println(s"Can't convert  $v  to  $typ") }

	def tryCast[T](code: => T): T = code

}