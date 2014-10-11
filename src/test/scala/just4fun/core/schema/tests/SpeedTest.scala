package just4fun.core.schema.tests


import just4fun.core.schema.{Schema}
import Predef.{println => prn, _}
import System.{currentTimeMillis => now}
import Runtime.{getRuntime => rtm}

object SpeedTest extends App {

//	// 135 ms
//	measureSpeedOf("Create Demo schema", 1) {
//		new Demo
//	}
//	// 95 ms
//	measureSpeedOf("Create Reflect schema", 1) {
//		val rClas = classOf[Reflect]
//		val rFields = classOf[Reflect].getDeclaredFields
//		rFields.foreach(_.setAccessible(true))
//	}


	class Demo extends BaseSchema[Demo] {
		type OBJ = DemoObj
		override val theName: String = "Demo"
		override protected def newObject: DemoObj = new DemoObj

		val p0 = Str_prop()("p0")
		val p1 = Str_prop()("p1")
		val p2 = Str_prop()("p2")
		val p3 = Str_prop()("p3")
		val p4 = Str_prop()("p4")
		val p5 = Str_prop()("p5")
		val p6 = Str_prop()("p6")
		val p7 = Str_prop()("p7")
		val p8 = Str_prop()("p8")
		val p9 = Str_prop()("p9")

		class DemoObj extends Obj
	}

	val demo = new Demo
	val valsSeq = List("prop 0", "prop 1", "prop 2", "prop 3", "prop 4", "prop 5", "prop 6", "prop 7", "prop 8", "prop 9")
	val obj = demo().setValuesArray(valsSeq)
	var n = -1
	val valsMap = obj.getValuesMap
	val valsArr = valsSeq.toArray
	var o: Demo#DemoObj = null
	var s: List[Any] = null

	class Reflect {
		var p0, p1, p2, p3, p4, p5, p6, p7, p8, p9 = null: String
	}
	val rClas = classOf[Reflect]
	val rFields = classOf[Reflect].getDeclaredFields
	rFields.foreach(_.setAccessible(true))
	val rObj = new Reflect
	rFields.foreach { f => f.set(rObj, valsMap(f.getName)) }

	def measureSpeedOf(msg: String, N: Int = 100000)(code: => Unit) = {
		val t = now
		for (n <- 0 until N) code
		prn(s"$msg // TIME = ${now - t }")
	}

	/* test Reflect */

	//130 (per 100000)
	def tReflectCreate = measureSpeedOf("tReflectCreate") {
		val rObj = new Reflect
		rFields.foreach { f => f.set(rObj, valsMap(f.getName)) }
	}
	//235 (per 100000)
	def tReflectGet = measureSpeedOf("tReflectGet") {
		val m = collection.mutable.Map[String, Any]()
		rFields.foreach { f => m(f.getName) = f.get(rObj) }
	}

	/* test set Values */

	//110 (per 100000)
	def tCopyObject = measureSpeedOf("tCopyObject") {
		o = obj.copy()
	}
	//95 (per 100000)
	def tSetValsArray = measureSpeedOf("tSetValsArray") {
		o = demo().setValuesArray(valsSeq)
	}
	// 150 (per 100000)
	def tSetValuesIterating = measureSpeedOf("tSetValuesIterating") {
		o = demo().setValuesIterating { onNext =>
			valsSeq.foreach(v => onNext(p => v))
		}
	}
	//171 (per 100000)
	def tSetValsMap = measureSpeedOf("tSetValsMap") {
		o = demo().setValuesMap(valsMap)
	}
	// 180 (per 100000)
	def tSetValsAll = measureSpeedOf("tSetValsAll") {
		o = demo().setValuesAll((i, p) => valsSeq(i))
	}
	// 215 (per 100000)
	def tSetValuesFinding = measureSpeedOf("tSetValuesFinding") {
		o = demo().setValuesFinding { onFind =>
			valsMap.foreach { case (k, v) => onFind(_.name == k, p => v) }
		}
	}
	// 245 (per 100000)
	def tSetValuesManual = measureSpeedOf("tSetValuesManual") {
		import demo._
		o = demo()(p0, "p 0")(p1, "p 0")(p2, "p 0")(p3, "p 0")(p4, "p 0")(p5, "p 0")(p6, "p 0")(p7, "p 0")(p8, "p 0")(p9, "p 0")
	}

	/* test get values */

	//120 (per 100000)
	def tGetJustNewMap = measureSpeedOf("tGetJustNewMap") {
		val map = collection.mutable.Map[String, Any]()
	}
	// 170 (per 100000)
	def tGetValuesIterating = measureSpeedOf("tGetValuesIterating") {
		s = List()
		obj.getValuesIterating { onNext =>
			valsSeq.foreach(_ => onNext((p, v) => s = v :: s))
		}
	}
	// 175 (per 100000)
	def tGetValsAll = measureSpeedOf("tGetValsAll") {
		s = List()
		obj.getValuesAll((i, p, v) => s = v :: s)
	}
	//135 (per 100000)
	def tGetValsArray = measureSpeedOf("tGetValsArray") {
		val s = obj.getValuesArray
	}
	// 370 (per 100000)
	def tGetValuesFinding = measureSpeedOf("tGetValuesFinding") {
		val map = collection.mutable.Map[String, Any]()
		obj.getValuesFinding { onFind =>
			demo.propsAll.foreach { p => onFind(_.name == p.name, { (_, v) => map(p.name) = v }) }
		}
	}
	//482 (per 100000)
	def tGetValsMap = measureSpeedOf("tGetValsMap") {
		val s = obj.getValuesMap
	}

	val arr = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)
	// 8  (per 1000000 ops)
	def tWriteArray = measureSpeedOf("tWriteArray") { arr(9) = 100 }
	// 7  (per 1000000 ops)
	def tReadArray = measureSpeedOf("tReadArray") { val x = arr(9) }


	def testSetValues: Unit = {
		tCopyObject // TIME = 9
		tSetValsArray // TIME = 18
		tSetValuesIterating // TIME = 27
		tSetValsAll // TIME = 28
		tSetValuesManual // TIME = 40
		tReflectCreate // TIME = 42
		tSetValsMap // TIME = 70
		tSetValuesFinding // TIME = 90
		prn()
	}
	def testGetValues: Unit = {
		tGetJustNewMap // TIME = 4
		tGetValsAll // TIME = 23
		tGetValsArray // TIME = 28
		tReflectGet // TIME = 32
		tGetValuesIterating // TIME = 34
		tGetValuesFinding // TIME = 95
		tGetValsMap // TIME = 152
		prn()
	}

//	tGetValsMap // TIME = 482
//	tSetValsMap // TIME = 171
//	tCopyObject // TIME = 110
//	tSetValsArray // TIME = 97
//	tSetValuesIterating // TIME = 147
//	tSetValsAll // TIME = 183
//	tSetValuesManual // TIME = 230
//	tReflectCreate // TIME = 120
//	tSetValuesFinding // TIME = 220
//	tGetJustNewMap // TIME = 112
//	tGetValsAll // TIME = 184
//	tGetValsArray // TIME = 135
//	tReflectGet // TIME = 234
//	tGetValuesIterating // TIME = 172
//	tGetValuesFinding // TIME = 374

			testSetValues
			testGetValues
			testSetValues
			testGetValues
	//	prn()
}
