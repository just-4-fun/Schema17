package just4fun.core.schema.tests

import scala.collection.mutable.ArrayBuffer

import just4fun.core.schema._
import org.scalatest.{FreeSpec, Matchers}

class MixedUsage extends FreeSpec with Matchers {

	/* TESTS */

	"Test onLoad / onSave" - {
		val subSchema = new Subobj
		val subObj = subSchema()
		val demo = new DemoMix(subSchema)
		import demo._
		val obj = demo()
		//
		"Let's see which pairs are linked and which not" in {
			propsFields shouldEqual List(p8, p7, p6, p5, p2, p1, p0)
			// Those pairs are not linked due to type mismatch
			propsFields should (not contain p3 and not contain p4)
		}
		"Let's setValues" in {
			val vals = List(12, null, Array(1, 2, 3.9), Array(true, false), true, null, Array(subObj), Array(Array(true, false)), Array())
			obj.setValuesArray(vals)
		}
		"Pair schema.p0: String # obj.pp0: String" in {
			// On prop access row value is being replaced to converted value
			// so p0 row value is converted from int to String
			obj.getRawValue(p0) shouldEqual "12"
			obj.pp0 shouldEqual "12" // field value
			obj(p0) shouldEqual "12" // prop value
		}
		"Pair schema.p1: Int # obj.pp1: Int" in {
			obj.pp1 shouldEqual 0
			obj(p1) shouldEqual 0
			// if row value is null (empty) it's not converted to prop type
			obj.getRawValue(p1).asInstanceOf[AnyRef] shouldEqual null
		}
		"Pair schema.p2: Array[Int] # obj.pp2: Array[Int]" in {
			// Converted from Array[Double](1, 2, 3.9) to Array[Int](1, 2, 3)
			obj.getRawValue(p2) shouldEqual Array(1, 2, 3)
			obj.pp2 shouldEqual Array(1, 2, 3)
			// prop value same as raw value
			obj(p2) shouldEqual Array(1, 2, 3)
		}
		"Pair (! not linked) schema.p3: Array[Boolean] # obj.pp3: Array[Int]" in {
			obj.pp3 shouldEqual Array()
			obj(p3) shouldEqual Array(true, false)
		}
		"Pair (! not linked) schema.p4: String # obj.pp4: Boolean" in {
			obj.getRawValue(p4) shouldEqual true
			obj(p4) shouldEqual "true"
			// row value is updated to converted value  inside call to obj.apply
			obj.getRawValue(p4) shouldEqual "true"
			obj.pp4 shouldEqual false
		}
		"Pair schema.p5: String # obj.pp5: String" in {
			// overwritten in onLoadInternal when set values
			obj.pp5 should (equal(null) and not equal "nothing")
			obj.getRawValue(p5) shouldEqual null.asInstanceOf[String]
			// prop value same as raw value
			obj(p5) shouldEqual null
		}
		"Pair schema.p6: Array[Subobj#OBJ] # obj.pp6: Array[Subobj#OBJ]" in {
			obj.getRawValue(p6) shouldEqual Array(subObj)
			obj(p6) shouldEqual Array(subObj)
			obj.pp6 shouldEqual Array(subObj)
		}
		"Pair schema.p7: Array[Array[Boolean]] # obj.pp7: Array[Array[Boolean]]" in {
			obj.getRawValue(p7) shouldEqual Array(Array(true, false))
			obj(p7) shouldEqual Array(Array(true, false))
			obj.pp7 shouldEqual Array(Array(true, false))
		}
		"Pair schema.p8: Subobj#OBJ # obj.pp8: Subobj#OBJ" in {
			obj.getRawValue(p8) shouldEqual subObj
			obj(p8) shouldEqual subObj
			obj.pp8 shouldEqual subObj
		}
		"When change prop the field is updated" in {
			obj(p0) = "full"
			// SEE: volume field is final but changed
			obj.pp0 shouldEqual "full"
		}
		"When change field the prop is NOT updated" in {
			obj.pp1 = 14
			obj(p1) shouldEqual 0
		}
		"When getValues.. is called prop values are updated to field values" - {
			"Call  getValues" in {
				obj.pp2 = Array(5, 2, 1)
				obj.pp3 = Array(5, 2, 1)
				obj.getValuesAll((n, p, v) => ())
			}
			"Props are actually updated" in {
				obj(p1) shouldEqual 14
				obj(p2) shouldEqual Array(5, 2, 1)
				// prop that was not linked is not updates
				obj(p3) shouldEqual Array(true, false)
			}
		}
	}

	/* PERFORMANCE TEST */

	"Performance comparison" in {
		import java.lang.System.{currentTimeMillis => now}

		val subSchema = new Subobj
		val subObj = subSchema()
		val demoMix = new DemoMix(subSchema)
		val demoSimple = new DemoSimple(subSchema)
		val vals = List(12, null, Array(1, 2, 3.9), Array(true, false), true, null, Array(subObj), Array(Array(true, false)), Array())

		var t = now
		val N = 1000000
		val U = 80

		def testMixedObj {
			def createObj: Unit = {
				val obj = demoMix()
				obj.setValuesArray(vals)
				for (n <- 0 until U) {
					val s = obj.pp0 // access
					obj.pp1 = 100 // update
				}
				obj.getValuesAll((n, p, v) => ())
			}
			t = now
			for (n <- 0 until N) createObj
			println(s"MIXED TIME= ${now - t }")
		}

		def testSimpleObj {
			import demoSimple._
			def createObj: Unit = {
				val obj = demoSimple()
				obj.setValuesArray(vals)
				for (n <- 0 until U) {
					val s = obj(p0) // access
					obj(p1) = 100 // update
				}
				obj.getValuesAll((n, p, v) => ())
			}
			t = now
			for (n <- 0 until N) createObj
			println(s"SIMPLE TIME= ${now - t }")
		}
		// Execute
		// NOTE: place what is measured first
		testSimpleObj
		testMixedObj
		//
		/** Conclusion:
		When U = 0 (not intensive obj usage) "Just prop" usage is 6 times faster than "mixed prop-field" usage
		  testSimpleObj// 420 ms per 1000000 objects
		  testMixedObj// 2450 ms per 1000000 objects
		When U = 80 (intensive obj usage: 80 reads + 80 writes) "Just prop" usage is about the same as "mixed prop-field" usage.
		  testSimpleObj// 2520 ms per 1000000 objects
		  testMixedObj// 2500 ms per 1000000 objects
		  */
	}

}

/* DEFINITIONS */

trait BaseSchemaMx[S <: BaseSchemaMx[S]] extends Schema[S] {this: S =>
	type PROP[T] = PropExt[T]
	override def newProp[T: Manifest](index: Byte, name: String, typ: PropType[T] with BaseType[_]): PROP[T] = new PropExt[T](index, name, typ)

	class PropExt[T: Manifest](val index: Byte, val name: String, val valueType: PropType[T] with BaseType[_]) extends Prop[T]
}

class DemoSimple(subSchema: Subobj) extends BaseSchemaMx[DemoSimple] {
	override type OBJ = DemoObj
	override protected def newObject: OBJ = new DemoObj
	override val theName: String = "DemoSimple"

	val p0 = Str_prop()("pp0")
	val p1 = Int_prop()("pp1")
	val p2 = Arr_prop()("pp2", IntType)
	val p3 = Arr_prop()("pp3", BooleanType)
	val p4 = Str_prop()("pp4")
	val p5 = Str_prop()("pp5")
	val p6 = Arr_prop()("pp6", new ObjectType(subSchema))
	val p7 = Arr_prop()("pp7", new ArrayType(BooleanType))
	val p8 = Obj_prop()("pp8", subSchema)

	class DemoObj extends Obj
}

class DemoMix(subSchema: Subobj) extends BaseSchemaMx[DemoMix] {
	override type OBJ = DemoObj
	override protected def newObject: OBJ = new DemoObj
	override val theName: String = "DemoMix"

	val p0 = Str_prop()("pp0")
	val p1 = Int_prop()("pp1")
	val p2 = Arr_prop()("pp2", IntType)
	val p3 = Arr_prop()("pp3", BooleanType)
	val p4 = Str_prop()("pp4")
	val p5 = Str_prop()("pp5")
	val p6 = Arr_prop()("pp6", new ObjectType(subSchema))
	val p7 = Arr_prop()("pp7", new ArrayType(BooleanType))
	val p8 = Obj_prop()("pp8", subSchema)


	class DemoObj extends Obj {
		// 'val' field values can be changed via linked props
		val pp0 = "ok"
		var pp1 = 11
		var pp2: Array[Int] = null
		// Field will not be linked: Generic type mismatch (schema.p3: Boolean VS object.pp3: Int).
		var pp3 = Array[Int]()
		// Field will not be linked: Type mismatch (schema.p4: String VS object.pp4: Boolean).
		var pp4 = false
		val pp5 = "nothing"
		var pp6: Array[Subobj#OBJ] = null
		val pp7: Array[Array[Boolean]] = null
		var pp8: Subobj#OBJ = null
	}
}

class Subobj extends BaseSchemaMx[Subobj] {
	override type OBJ = Subobj
	override protected def newObject = new Subobj
	override val theName: String = "Subobj"

	val p0 = Str_prop()("pp0")

	class Subobj extends Obj {
		val pp0 = "ok"
	}
}



