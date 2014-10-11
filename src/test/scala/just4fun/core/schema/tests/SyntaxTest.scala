package just4fun.core.schema.tests

import java.math.BigInteger
import just4fun.core.schema.formatters.JsonArrayFormatter

import scala.Predef.{println => prn, _}
import java.lang.System.{currentTimeMillis => now}
import just4fun.core.schema._

import scala.collection.mutable.ArrayBuffer

object TestSyntax extends App {

	import SchemaImplicits._

	val s3 = new Sch3
	val s2 = new Sch2
	val s1 = new Sch1(s2)

	val o1s1: Sch1#Obj1 = s1()

	o1s1(s1.p0) = "Prop 0"
	val v1o1s1: String = o1s1(s1.p0)
	//	o1s1(Sch1.p0) = 12
	//	val v2o1s1: Long = o1s1(Sch1.p0)

	o1s1(s1.p1) = true
	val v3o1s1: Boolean = o1s1(s1.p1)

	o1s1(s1.p2) = 11L
	val v4o1s1: Long = o1s1(s1.p2)

	o1s1(s1.p3) = s2()
	//			o1s1(Sch1.p3) = Sch1()
	//			val o3s1: Sch1.Obj1 = o1s1(Sch1.p3)
	val o4s1: Sch2#Obj2 = o1s1(s1.p3)

	o1s1(s1.p4) = Array(false, true)
	//		o1s1(Sch1.p4) = Array(1, 2)
	//		val a1: Array[Int] = o1s1(Sch1.p4)
	val a1: Array[Boolean] = o1s1(s1.p4)

	o1s1(s1.p5) = Array(s2(), s2())
	//			o1s1(Sch1.p5) = Array(false, true)
	//	val a3: Array[Sch1.Obj1] = o1s1(Sch1.p5)
	val a4: Array[Sch2#Obj2] = o1s1(s1.p5)

	o1s1(s1.p6) = new BigInt(new BigInteger("110123230201238972983172938712938729837291378921330"))
	val vBig1: BigInt = o1s1(s1.p6)
	// implicit Prop to typ usage
	prn(s"p6= ${s1.p6.asString(vBig1) }")

	// Chaining assignment syntax
	{
		import s1._
		val obj = s1()(p0, "ok")(p1, true)(p2, 1000L)(p3, s2())
		prn(s"obj >> $obj")
	}

	o1s1.getValuesLive { (n, p, v) =>
		prn(p.text)
	}

	def test1[T <: Schema[T]](s: T): T#OBJ = {
		val obj = s()
		s.propsAll.foreach(p => prn(s"${p.index } - ${p.name };  v= ${obj(p) }"))
		obj
	}
	def test2[T <: Schema[T]](s: T)(obj: T#OBJ): Unit = {
		obj.getValuesOf(obj.scheme.propsAll.filter(!_.isSTUB)) { (n, p, v) => 11 }
		obj.getValuesAll { (n, p, v) => 11 }
	}
	def test2_[T <: Schema[T]](obj: T#OBJ): Unit = {
		obj.getValuesOf(obj.scheme.propsAll.filter(!_.isSTUB)) { (n, p, v) => 11 }
		obj.getValuesAll { (n, p, v) => 11 }
	}
	def test3[V, T <: Schema[T]](s: T)(obj: T#OBJ, p: T#PROP[V], v: V): Unit = {
		obj(p) = v
	}
	def test3_[V, T <: Schema[T]](obj: T#OBJ, p: T#PROP[V], v: V): Unit = {
		obj(p) = v
	}
	def test4[T <: Schema[T]](s: T)(obj: T#OBJ, pps: Iterable[T#PROP[_]]): Unit = {
		obj.getValuesOf(pps) { (n, p, v) => 11 }
	}
	def test4_[T <: Schema[T]](obj: T#OBJ, pps: Iterable[T#PROP[_]]): Unit = {
		obj.getValuesOf(pps) { (n, p, v) => 11 }
	}
	def test4__[T <: Schema[T]](obj: T#OBJ, pps: Iterable[T#PROP[_]]): Unit = {
		obj.getValuesOf(pps) { (n, p, v) => 11 }
	}


	val o: Sch1#OBJ = s1().setValuesArray(List("ok", true, 1001, o1s1, null, List(o1s1), "12345"))
	val o2 = s1() setValuesArray List("ok", true, 1001, null, null, null, "12345")
	test2(s1)(o)
	test2_[Sch1](o)
	test3(s1)(o, s1.p0, "ok")
	test3_[String, Sch1](o, s1.p0, "ok")
	test3_(s1(), s1.p0, "ok")
	test3_(s2(), s2.p0, "ok")
	val props = List(s1.p0, s1.p1)
	test4(s1)(o, props)
	//	test4_(o, props)
	test4__[Sch1](o, props)
	//	test4(o, props)

	prn(s"obj.toString >> $o")
	prn(s"Equals ? ${o == o2 }")
	prn(s"Hash o= ${o.hashCode };  o2=${o2.hashCode }")

	prn(s"Sch1 >> $s1")
	prn(s"Sch2 >> $s2")
	prn(s"Sch3 >> $s3")

	o1s1.getValuesAll { (n, p, v) => 
		prn(s"Prop ${p.valueType }")
		p.valueType match {
			case t: StringBaseType => writeString(t.asBaseType(v))
			case t: DoubleBaseType => writeDouble(t.asBaseType(v))
			case t: LongBaseType => writeLong(t.asBaseType(v))
			case t: BooleanBaseType => writeBoolean(t.asBaseType(v))
			case t: BinaryBaseType => writeBinary(t.asBaseType(v))
			case t: ObjectBaseType[_] => writeObject(t.asBaseType(v))
			case t: ArrayBaseType => writeArray(t.asBaseType(v))
			case _ =>
		}
	}
	def writeString(v: String) { prn(s"asStr= $v") }
	def writeLong(v: Long) { prn(s"asLng= $v") }
	def writeDouble(v: Double) { prn(s"asDbl= $v") }
	def writeBoolean(v: Boolean) { prn(s"asBoo= $v") }
	def writeBinary(v: Array[Byte]) { prn(s"asBin= $v") }
	def writeObject(v: Iterable[_]) { prn(s"asMap= $v") }
	def writeArray(v: Iterable[_]) { prn(s"asArr= $v") }


	prn("ok")

}


/* DEFINITIONS */


class Sch1(sch2: Sch2) extends BaseSchema[Sch1] {
	type OBJ = Obj1
	val theName: String = "Sch 1"
	val p0 = Str_prop()("p0")
	val p1 = Boo_prop()("p1")
	val p2 = Lng_prop()("p2")
	val p3 = Obj_prop()("p3", sch2)
	val p4 = Arr_prop()("p4", BooleanType)
	val p5 = Arr_prop()("p5", new ObjectType(sch2))
	val p6 = Bigi_prop()("p6", { _.text = "ok" })
	def newObject: Obj1 = new Obj1
	class Obj1 extends BaseObj
}
class Sch2 extends BaseSchema[Sch2] {
	type OBJ = Obj2
	val theName: String = "Sch 2"
	val p0 = Str_prop()("p0")
	val p1 = Boo_prop()("p1")
	val p2 = Lng_prop()("p2")
	def newObject: Obj2 = new Obj2
	class Obj2 extends BaseObj
}
class Sch3 extends BaseSchema[Sch3] {
	type OBJ = Obj3
	val theName: String = "Sch 3"
	val p0 = Str_prop()("p0")
	val p1 = Boo_prop()("p1")
	val p2 = Lng_prop()("p2", _.text = "ok")
	def newObject: Obj3 = new Obj3
	class Obj3 extends BaseObj
}

//object Sch4 extends BaseSchema[Sch4.type ] {
//	type OBJ = Obj3
//	val theName: String = "Sch 3"
//	val p0 = Str_prop()("p0")
//	val p1 = Boo_prop()("p1")
//	val p2 = Lng_prop()("p2", _.text = "ok")
//	def newObject: Obj3 = new Obj3
//	class Obj3 extends BaseObj
//}


