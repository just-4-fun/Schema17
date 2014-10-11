import scala.Predef.{println => prn, _}
import java.lang.System.{currentTimeMillis => now}
import just4fun.core.schema._

import scala.collection.mutable.ArrayBuffer


object Test extends App{

	val sch = new Subobj

	def toArray[T<:Schema[T]](v: Any) = {
		v match {
			case v: Array[String] => prn(s"Array [String]= ${v.mkString(",")}")
			case v: Array[Boolean] => prn(s"Array [Boolean]= ${v.mkString(",")}")
			case v: Array[Array[_]] => prn(s"Array [Array]= ${v.mkString(",")}")
			case v: Array[List[_]] => prn(s"Array [List]= ${v.mkString(",")}")
			case v: Array[T#OBJ] => prn(s"Array [Obj]= ${v.mkString(",")}")
			case _ => prn("Other ", v)
		}
	}

	toArray(Array(1, 2, 3))
	toArray(Array("a", "b"))
	toArray(Array(true, false))
	toArray(Array(Array(true, false)))
	toArray(Array(List(true, false)))
	toArray(Array(sch()))
//	var t = now
//	val N = 1000000
//	def testSample {
//		t = now
//		prn(s"TIME= ${now-t}")
//	}
}


trait BaseDemo[S<:BaseDemo[S]] extends Schema[S] {this: S =>
	type PROP[T] = PropExt[T]
	override def newProp[T: Manifest](index: Byte, name: String, typ: PropType[T] with BaseType[_]): PROP[T] = new PropExt[T](index, name, typ)

	class PropExt[T: Manifest](val index: Byte, val name: String, val valueType: PropType[T] with BaseType[_]) extends Prop[T]
}
class Subobj extends BaseDemo[Subobj] {
	override type OBJ = Subobj
	override protected def newObject = new Subobj
	override val theName: String = "Subobj"

	val p0 = Str_prop()("pp0")

	class Subobj extends Obj {
		val pp0 = "ok"
	}
}
