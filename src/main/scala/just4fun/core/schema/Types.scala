package just4fun.core.schema


/* PROP TYPE */

abstract class PropType[T](implicit val mft: Manifest[T]) {
	protected implicit val utils: TypeUtils with Formatter
	def evaluate(v: Any): T
	def asString(v: Any): String = utils.any2string(v)
	def asBytes(v: Any): Array[Byte] = utils.any2bytes(v)
	def asLong(v: Any): Long = utils.any2long(v)
	def asDouble(v: Any): Double = utils.any2double(v)
	def asBoolean(v: Any): Boolean = utils.any2boolean(v)
	def default: T = null.asInstanceOf[T]
}


/* PROP TYPE classes */

abstract class StringPropType extends PropType[String] {
	def evaluate(v: Any): String = asString(v)
}
abstract class BinaryPropType extends PropType[Array[Byte]] {
	def evaluate(v: Any): Array[Byte] = asBytes(v)
}
abstract class LongPropType extends PropType[Long] {
	def evaluate(v: Any): Long = asLong(v)
	override def default = 0L
}
abstract class IntPropType extends PropType[Int] {
	def evaluate(v: Any): Int = utils.any2int(v)
	override def default = 0
}
abstract class DoublePropType extends PropType[Double] {
	def evaluate(v: Any): Double = asDouble(v)
	override def default = 0d
}
abstract class FloatPropType extends PropType[Float] {
	def evaluate(v: Any): Float = utils.any2float(v)
	override def default = 0f
}
abstract class BooleanPropType extends PropType[Boolean] {
	def evaluate(v: Any): Boolean = asBoolean(v)
	override def default = false
}
abstract class ObjectPropType[T<:Schema[T]](implicit override val mft: Manifest[T#OBJ]) extends PropType[T#OBJ] {
	val schema: T
	val castFn: Any => T#OBJ = utils.any2objectFn(schema)
	def evaluate(v: Any): T#OBJ = castFn(v)
	override def asString(v: Any): String = v match {
		case v: String => v
		case null => null
		case _ => val obj = evaluate(v); utils.stringFromObj(schema)(obj)
	}
	override def asBytes(v: Any): Array[Byte] = v match {
		case v: Array[Byte] => v
		case null => null
		case _ => val obj = evaluate(v); utils.bytesFromObj(schema)(obj)
	}
}
abstract class ArrayPropType[T: Manifest] extends PropType[Array[T]] {
	val elementType: PropType[T] with BaseType[_]
	val castFn: Any => Array[T] = utils.any2arrayFn(elementType)
	def evaluate(v: Any): Array[T] = castFn(v)
	override def asString(v: Any): String = v match {
		case v: String => v
		case null => null
		case _ => utils.stringFromArray(evaluate(v))
	}
	override def asBytes(v: Any): Array[Byte] = v match {
		case v: Array[Byte] => v
		case null => null
		case _ => utils.bytesFromArray(evaluate(v))
	}
}





/* BASE TYPE  */

sealed trait BaseType[T] {this: PropType[_] =>
	def asBaseType(v: Any, arrayContext: Boolean = true): T
}

/* 1  LEVEL baseTypes  (sealed) */

sealed trait StreamBaseType[T] extends BaseType[T] {this: PropType[_] =>}
sealed trait NumberBaseType[T] extends BaseType[T] {this: PropType[_] =>}
sealed trait IterableBaseType extends BaseType[Iterable[_]] {this: PropType[_] =>}

/* 2  LEVEL baseTypes (sealed) */

trait StringBaseType extends StreamBaseType[String] {this: PropType[_] =>
	def asBaseType(v: Any, arrayContext: Boolean = true) = asString(v)
}
trait BinaryBaseType extends StreamBaseType[Array[Byte]] {this: PropType[_] =>
	def asBaseType(v: Any, arrayContext: Boolean = true) = asBytes(v)
}
trait LongBaseType extends NumberBaseType[Long] {this: PropType[_] =>
	def asBaseType(v: Any, arrayContext: Boolean = true) = asLong(v)
}
trait DoubleBaseType extends NumberBaseType[Double] {this: PropType[_] =>
	def asBaseType(v: Any, arrayContext: Boolean = true) = asDouble(v)
}
trait BooleanBaseType extends NumberBaseType[Boolean] {this: PropType[_] =>
	def asBaseType(v: Any, arrayContext: Boolean = true) = asBoolean(v)
}
trait ObjectBaseType[T<:Schema[T]] extends IterableBaseType {this: ObjectPropType[T] =>
	def asBaseType(v: Any, arrayContext: Boolean = true): Iterable[_] = v match {
		case v: T#OBJ => if (arrayContext) v.getValuesArray else v.getValuesMap
		case null => null
		case _ => asBaseType(evaluate(v))
	}
}
trait ArrayBaseType extends IterableBaseType {this: ArrayPropType[_] =>
	def asBaseType(v: Any, arrayContext: Boolean = true): Iterable[_] = v match {
		case v: Array[_] => v.map(e => elementType.asBaseType(e))
		case null => null
		case _ => asBaseType(evaluate(v))
	}
}







/* PROP & BASE Type mixins  */

class StringType(implicit val utils: TypeUtils with Formatter)
  extends StringPropType with StringBaseType

class BinaryType(implicit val utils: TypeUtils with Formatter)
  extends BinaryPropType with BinaryBaseType

class LongType(implicit val utils: TypeUtils with Formatter)
  extends LongPropType with LongBaseType

class IntType(implicit val utils: TypeUtils with Formatter)
  extends IntPropType with LongBaseType

class DoubleType(implicit val utils: TypeUtils with Formatter)
  extends DoublePropType with DoubleBaseType

class FloatType(implicit val utils: TypeUtils with Formatter)
  extends FloatPropType with DoubleBaseType

class BooleanType(implicit val utils: TypeUtils with Formatter)
  extends BooleanPropType with BooleanBaseType

class ObjectType[T<:Schema[T]](val schema: T)
  (implicit val utils: TypeUtils with Formatter, override val mft: Manifest[T#OBJ])
  extends ObjectPropType[T] with ObjectBaseType[T]

class ArrayType[T](val elementType: PropType[T] with BaseType[_])
  (implicit val utils: TypeUtils with Formatter, val m: Manifest[T])
  extends ArrayPropType[T] with ArrayBaseType

class StubType(implicit val utils: TypeUtils with Formatter)
  extends StringPropType with StringBaseType {
	override def asBaseType(v: Any, arrayContext: Boolean = true): String = null
	override def asString(v: Any): String = null
	override def asBytes(v: Any): Array[Byte] = null
	override def asLong(v: Any): Long = 0L
	override def asDouble(v: Any): Double = 0d
	override def asBoolean(v: Any): Boolean = false
}
