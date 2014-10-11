package just4fun.core.schema.tests

import java.math.BigInteger

import just4fun.core.schema._
import just4fun.core.schema.formatters.JsonArrayFormatter

trait BaseSchema[S <: BaseSchema[S]] extends Schema[S] {this: S =>
	type PROP[T] = PropExt[T]
	override implicit val utils = new JsonArrayFormatter with TypeUtils {}

	override val theVersion = 1

	override def newProp[T: Manifest](index: Byte, name: String, typ: PropType[T] with BaseType[_]): PROP[T] = new PropExt[T](index, name, typ)


	trait BaseObj extends Obj {self: OBJ =>}

	class PropExt[T: Manifest](val index: Byte, val name: String, val valueType: PropType[T] with BaseType[_]) extends Prop[T] {
		var text: String = _
	}

	def Bigi_prop(index: Byte = propsSize)(name: String, initCode: S#PROP[_] => Unit = null): S#PROP[BigInt] = constructProp(index, name, new PropType[BigInt] with StringBaseType {
		implicit val utils = BaseSchema.this.utils
		override def evaluate(v: Any): BigInt = v match {
			case v: BigInt => v
			case null => new BigInt(new BigInteger("0"))
			case _ => new BigInt(new BigInteger(asString(v)))
		}
		override def asString(v: Any): String = v match {
			case v: BigInt => v.toString(10)
			case _ => super.asString(v)
		}
		override def asBaseType(v: Any, arrayContext: Boolean = true): String = asString(v)
	}, initCode)
}
