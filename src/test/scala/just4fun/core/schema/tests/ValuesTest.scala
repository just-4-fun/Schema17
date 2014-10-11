package just4fun.core.schema.tests


import just4fun.core.schema._
import org.scalatest.{Matchers, FreeSpec}
import Predef.{println => prn, _}
import System.{currentTimeMillis => now}
import Runtime.{getRuntime => rtm}

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

class ValuesTest extends FreeSpec with Matchers {

	class Sch1(sch2: Sch2) extends BaseSchema[Sch1] {
		type OBJ = Obj1
		val theName: String = "Sch 1"
		val p0 = Str_prop()("p0")
		val p1 = Boo_prop()("p1")
		val p2 = Lng_prop()("p2")
		val p3 = Obj_prop()("p3", sch2)
		val p4 = Arr_prop()("p4", new BooleanType)
		val p5 = Arr_prop()("p5", new ObjectType(sch2))
		val p6 = Bigi_prop()("p6")
		def newObject: Obj1 = new Obj1
		class Obj1 extends Obj
	}

	class Sch2 extends BaseSchema[Sch2] {
		type OBJ = Obj2
		val theName: String = "Sch 2"
		val p0 = Str_prop()("p0")
		val p1 = Boo_prop()("p1")
		val p2 = Lng_prop()("p2")
		def newObject: Obj2 = new Obj2
		class Obj2 extends Obj
	}
	class Sch3 extends BaseSchema[Sch3] {
		type OBJ = Obj3
		val theName: String = "Sch 3"
		val p0 = Str_prop()("p0")
		val p1 = Boo_prop()("p1")
		val p2 = Lng_prop()("p2")
		def newObject: Obj3 = new Obj3
		class Obj3 extends Obj
	}

	val sch3 = new Sch3
	val sch2 = new Sch2
	val sch1 = new Sch1(sch2)


	/* tests */

	"Test Set / Get Values" - {
		val bools = List(true, false)
		val s2vals1 = Vector("subObj1", true, 1002)
		val s2vals2 = Vector("subObj2", false, 1003)
		val s2vals3 = Vector("subObj3", true, 1004)
		val s2valsAll = List(s2vals2, s2vals3)
		val s2o1 = sch2().setValuesArray(s2vals1)
		val s2o2 = sch2().setValuesArray(s2vals2)
		val s2o3 = sch2().setValuesArray(s2vals3)
		val map = Map("p0" -> "ok", "p1" -> true, "p2" -> 1001, "p3" -> s2vals1, "p4" -> bools, "p5" -> s2valsAll, "p6" -> "12345")
		val seq = Vector("ok", true, 1001, s2vals1, bools, s2valsAll, "12345")
		val props1 = List(sch1.p6, sch1.p5, sch1.p4, sch1.p3, sch1.p2, sch1.p1, sch1.p0)
		val s1o1 = sch1().setValuesArray(seq)

		/* SET VALUES */
		"When setValuesArray" in {
			val obj = sch1().setValuesArray(seq)
			prn(s"obj= $obj")
			obj(sch1.p0) shouldEqual "ok"
			obj(sch1.p1) shouldEqual true
			obj(sch1.p2) shouldEqual 1001
			obj(sch1.p3) shouldEqual s2o1
			obj(sch1.p4) shouldEqual ArrayBuffer(true, false)
			obj(sch1.p5) shouldEqual ArrayBuffer(s2o2, s2o3)
			obj(sch1.p6) shouldEqual 12345
		}
		"When setValuesMap" in {
			val obj = sch1().setValuesMap(map)
			prn(s"obj= $obj")
			obj(sch1.p0) shouldEqual "ok"
			obj(sch1.p1) shouldEqual true
			obj(sch1.p2) shouldEqual 1001
			obj(sch1.p3) shouldEqual s2o1
			obj(sch1.p4) shouldEqual ArrayBuffer(true, false)
			obj(sch1.p5) shouldEqual ArrayBuffer(s2o2, s2o3)
			obj(sch1.p6) shouldEqual 12345
		}
		"When setValuesOf" in {
			val obj = sch1().setValuesOf(props1) { (n, p) => map(p.name) }
			prn(s"obj= $obj")
			obj(sch1.p0) shouldEqual "ok"
			obj(sch1.p1) shouldEqual true
			obj(sch1.p2) shouldEqual 1001
			obj(sch1.p3) shouldEqual s2o1
			obj(sch1.p4) shouldEqual ArrayBuffer(true, false)
			obj(sch1.p5) shouldEqual ArrayBuffer(s2o2, s2o3)
			obj(sch1.p6) shouldEqual 12345
		}
		"When setValuesAll" in {
			val obj = sch1().setValuesAll { (n, p) => seq(n) }
			prn(s"obj= $obj")
			obj(sch1.p0) shouldEqual "ok"
			obj(sch1.p1) shouldEqual true
			obj(sch1.p2) shouldEqual 1001
			obj(sch1.p3) shouldEqual s2o1
			obj(sch1.p4) shouldEqual ArrayBuffer(true, false)
			obj(sch1.p5) shouldEqual ArrayBuffer(s2o2, s2o3)
			obj(sch1.p6) shouldEqual 12345
		}
		"When setValuesLive" in {
			val obj = sch1().setValuesLive { (n, p) => map(p.name) }
			prn(s"obj= $obj")
			obj(sch1.p0) shouldEqual "ok"
			obj(sch1.p1) shouldEqual true
			obj(sch1.p2) shouldEqual 1001
			obj(sch1.p3) shouldEqual s2o1
			obj(sch1.p4) shouldEqual ArrayBuffer(true, false)
			obj(sch1.p5) shouldEqual ArrayBuffer(s2o2, s2o3)
			obj(sch1.p6) shouldEqual 12345
		}
		"When setValuesIterating" in {
			val obj = sch1().setValuesIterating { onNextProp =>
				seq.foreach { v =>
					onNextProp(p => v)
				}
			}
			prn(s"obj= $obj")
			obj(sch1.p0) shouldEqual "ok"
			obj(sch1.p1) shouldEqual true
			obj(sch1.p2) shouldEqual 1001
			obj(sch1.p3) shouldEqual s2o1
			obj(sch1.p4) shouldEqual ArrayBuffer(true, false)
			obj(sch1.p5) shouldEqual ArrayBuffer(s2o2, s2o3)
			obj(sch1.p6) shouldEqual 12345
		}
		"When setValuesFinding" in {
			val obj = sch1().setValuesFinding { onFindProp =>
				map.foreach { case (k, v) =>
					onFindProp(p => p.name == k, p => v)
				}
			}
			prn(s"obj= $obj")
			obj(sch1.p0) shouldEqual "ok"
			obj(sch1.p1) shouldEqual true
			obj(sch1.p2) shouldEqual 1001
			obj(sch1.p3) shouldEqual s2o1
			obj(sch1.p4) shouldEqual ArrayBuffer(true, false)
			obj(sch1.p5) shouldEqual ArrayBuffer(s2o2, s2o3)
			obj(sch1.p6) shouldEqual 12345
		}

		/* GET VALUES */
		"When getValuesArray" in {
			val seq2 = s1o1.getValuesArray
			seq2 shouldEqual seq
			val o = sch1().setValuesArray(seq2)
			s1o1 shouldEqual o
		}
		"When getValuesMap" in {
			val map2 = s1o1.getValuesMap
			map2 shouldEqual map
			val o = sch1().setValuesMap(map2)
			s1o1 shouldEqual o
		}
		"When getValuesOf" in {
			val map2 = collection.mutable.Map[String, Any]()
			s1o1.getValuesOf(props1) { (n, p, v) => map2(p.name) = v }
			map2 shouldEqual map
		}
		"When getValuesAll" in {
			var seq2 = List[Any]()
			s1o1.getValuesAll { (n, p, v) => seq2 = v :: seq2 }
			seq2.reverse shouldEqual seq
		}
		"When getValuesLive" in {
			val map2 = collection.mutable.Map[String, Any]()
			s1o1.getValuesLive { (n, p, v) => map2(p.name) = v }
			map2 shouldEqual map
		}
		"When getValuesIterating" in {
			var seq2 = List[Any]()
			s1o1.getValuesIterating { onNextProp =>
				seq.foreach { _ =>
					onNextProp((p, v) => seq2 = v :: seq2)
				}
			}
			seq2.reverse shouldEqual seq
		}
		"When getValuesFinding" in {
			val map2 = collection.mutable.Map[String, Any]()
			s1o1.getValuesFinding { onFindProp =>
				map.foreach { case (k, _) =>
					onFindProp(p => p.name == k, (p, v) => map2(k) = v)
				}
			}
			map2 shouldEqual map
		}
	}

}
