package just4fun.core.schema.tests


import just4fun.core.schema.formatters.JsonArrayFormatter
import org.scalatest._
import just4fun.core.schema._

class CastingTest extends FreeSpec with Matchers {
	implicit val utils = new TypeUtils with JsonArrayFormatter
	import utils._

	class Anything { override def toString: String = "Anything else"}

	"String to double" - {
		"When D" in { string2double("qwe") shouldEqual 0 }
		"When D." in { string2double("qw.re") shouldEqual 0 }
		"When -D." in { string2double("q-w-.re") shouldEqual 0 }
		"When d" in { string2double("12") shouldEqual 12 }
		"When -d" in { string2double("-12") shouldEqual -12 }
		"When d.d" in { string2double("12.02") shouldEqual 12.02 }
		"When -d.d" in { string2double("-12.02") shouldEqual -12.02 }
		"When -d." in { string2double("-12.") shouldEqual -12 }
		"When .d" in { string2double(".12") shouldEqual 0.12 }
		"When -.d" in { string2double("-.12") shouldEqual -0.12 }
		"When -0.d" in { string2double("-0.12") shouldEqual -0.12 }
		"When Dd" in { string2double("qw12 34s") shouldEqual 12 }
		"When -" in { string2double("-12 .432 qwe") shouldEqual -12 }
		"When w-" in { string2double("qw-12 .432 qwe") shouldEqual -12 }
		"When -W" in { string2double("--x12. 432") shouldEqual 12 }
		"When - -d" in { string2double("- -12. 432") shouldEqual -12 }
		"When -W." in { string2double("-.432u") shouldEqual -0.432 }
		"When .." in { string2double("-12..432u") shouldEqual -12 }
		"When -.." in { string2double("-..42u") shouldEqual 0.42 }
		"When W . W" in { string2double("x12 . 55") shouldEqual 12 }
		"When d..." in { string2double("x12...") shouldEqual 12 }
		"When ...d" in { string2double("x..,12...") shouldEqual 0.12 }
		"When -...d" in { string2double("x-..,12...") shouldEqual 0.12 }
	}

	"Cast Boolean" - {
		"When true" in { any2boolean(true) shouldEqual true }
		"When false" in { any2boolean(false) shouldEqual false }
		"When 0" in { any2boolean(0) shouldEqual false }
		"When pos Int" in { any2boolean(12) shouldEqual true }
		"When neg Long" in { any2boolean(-12L) shouldEqual true }
		"When 0 Double" in { any2boolean(0.0d) shouldEqual false }
		"When 0 Float" in { any2boolean(0.0f) shouldEqual false }
		"When empty string" in { any2boolean("") shouldEqual false }
		"When Null string" in { any2boolean("Null") shouldEqual false }
		"When false string" in { any2boolean("FALSE") shouldEqual false }
		"When 0 string" in { any2boolean("0") shouldEqual false }
		"When 0.0 string" in { any2boolean(0.0d.toString) shouldEqual false }
		"When 0,0 string" in { any2boolean("0,0") shouldEqual false }
		"When null" in { any2boolean(null) shouldEqual false }
		"When bytes" in { any2boolean(Array[Byte](116, 114, 117, 101)) shouldEqual true }
		"When other" in { any2boolean(new Anything) shouldEqual true }
	}

	"Cast Int" - {
		"When Int" in { any2int(12) shouldEqual 12 }
		"When null" in { any2int(null) shouldEqual 0 }
		"When Long" in { any2int(123456789012345L) shouldEqual -2045911175 } // !!!
		"When Double" in { any2int(1766787687682.802211) shouldEqual 2147483647 } // !!!
		"When Float" in { any2int(-0.2110021f) shouldEqual 0 }
		"When num String" in { any2int("-12.55") shouldEqual -12 }
		"When mixed String" in { any2int("x12 . 55") shouldEqual 12 }
		"When true" in { any2int(true) shouldEqual 1 }
		"When false" in { any2int(false) shouldEqual 0 }
		"When short" in { any2int(14.toShort) shouldEqual 14 }
		"When byte" in { any2int(14.toByte) shouldEqual 14 }
		"When char" in { any2int('a') shouldEqual 97 }
		"When bytes" in { any2int(Array[Byte](49, 50, 53, 46, 49, 50)) shouldEqual 125 }
		"When other" in { any2int(new Anything) shouldEqual 0 }
	}

	"Cast Long" - {
		"When Int" in { any2long(12) shouldEqual 12 }
		"When null" in { any2long(null) shouldEqual 0 }
		"When Long" in { any2long(123456789012345L) shouldEqual 123456789012345L }
		"When Double" in { any2long(1766787687682.802211) shouldEqual 1766787687682L }
		"When Float" in { any2long(-0.2110021f) shouldEqual 0 }
		"When num String" in { any2long("-12.55") shouldEqual -12 }
		"When mixed String" in { any2long("x12 . 55") shouldEqual 12 }
		"When true" in { any2long(true) shouldEqual 1 }
		"When false" in { any2long(false) shouldEqual 0 }
		"When short" in { any2long(14.toShort) shouldEqual 14 }
		"When byte" in { any2long(14.toByte) shouldEqual 14 }
		"When char" in { any2long('a') shouldEqual 97 }
		"When bytes" in { any2long(Array[Byte](49, 50, 53, 46, 49, 50)) shouldEqual 125 }
		"When other" in { any2long(new Anything) shouldEqual 0 }
	}

	"Cast Float" - {
		"When Int" in { any2float(12) shouldEqual 12f }
		"When null" in { any2float(null) shouldEqual 0f }
		"When Long" in { any2float(123456789012345L) shouldEqual 123456788000000f } // !!!
		"When Double" in { any2float(1766787687682.802211) shouldEqual 1766787650000f } // !!!
		"When Float" in { any2float(-0.2110021f) shouldEqual -0.2110021f }
		"When num String" in { any2float("-12.55") shouldEqual -12.55f }
		"When mixed String" in { any2float("x12 . 55") shouldEqual 12f }
		"When true" in { any2float(true) shouldEqual 1f }
		"When false" in { any2float(false) shouldEqual 0f }
		"When short" in { any2float(14.toShort) shouldEqual 14f }
		"When byte" in { any2float(14.toByte) shouldEqual 14f }
		"When char" in { any2float('a') shouldEqual 97f }
		"When bytes" in { any2float(Array[Byte](49, 50, 53, 46, 49, 50)) shouldEqual 125.12f }
		"When other" in { any2float(new Anything) shouldEqual 0f }
	}

	"Cast Double" - {
		"When Int" in { any2double(12) shouldEqual 12 }
		"When null" in { any2double(null) shouldEqual 0 }
		"When Long" in { any2double(123456789012345L) shouldEqual 123456789012345d }
		"When Double" in { any2double(1766787687682.802211) shouldEqual 1766787687682.802211 }
		"When Float" in { any2double(-0.2110021f) shouldEqual -0.21100209653377533 } // !!!
		"When num String" in { any2double("-12.55") shouldEqual -12.55 }
		"When mixed String" in { any2double("x12 . 55") shouldEqual 12 }
		"When true" in { any2double(true) shouldEqual 1 }
		"When false" in { any2double(false) shouldEqual 0 }
		"When short" in { any2double(14.toShort) shouldEqual 14 }
		"When byte" in { any2double(14.toByte) shouldEqual 14 }
		"When char" in { any2double('a') shouldEqual 97 }
		"When bytes" in { any2double(Array[Byte](49, 50, 53, 46, 49, 50)) shouldEqual 125.12 }
		"When other" in { any2double(new Anything) shouldEqual 0 }
	}

	"Cast Bytes" - {
		"When bytes" in { any2bytes(Array[Byte](0, 1, 127)) shouldEqual Array[Byte](0, 1, 127) }
		"When null" in { any2bytes(null) shouldEqual null }
		"When string" in { any2bytes("abc") shouldEqual Array[Byte](97, 98, 99) }
		"When string of digits" in { any2bytes("010") shouldEqual Array[Byte](48, 49, 48) }
		"When string utf8" in { any2bytes("абв") shouldEqual Array[Byte](-48, -80, -48, -79, -48, -78) }
		"When true" in { any2bytes(true) shouldEqual Array[Byte](116, 114, 117, 101) }
		"When num" in { any2bytes(125.12) shouldEqual Array[Byte](49, 50, 53, 46, 49, 50) }
	}

	"Cast String" - {
		"When string" in { any2string("ычday") shouldEqual "ычday" }
		"When null" in { any2string(null) shouldEqual null }
		"When num" in { any2string(591.217) shouldEqual "591.217" }
		"When false" in { any2string(false) shouldEqual "false" }
		"When bytes" in { any2string(Array[Byte](-48, -80, -48, -79, -48, -78)) shouldEqual "абв" }
	}

	"Cast Array" - {
		"When self ref" in {
			val ref = Array("ok", "no")
			any2array(new StringType).apply(ref) shouldBe ref
		}
		"When null" in { any2array(new StringType).apply(null) shouldEqual null }
		"When Strings" in { any2array(new StringType).apply(List("ok", "no")) shouldEqual Array("ok", "no") }
		"When fake Strings" in { any2array(new StringType).apply(List(100, 200)) shouldEqual Array("100", "200") }
		"When Ints" in { any2array(new IntType).apply(List(1, 2, 3)) shouldEqual Array(1, 2, 3) }
		"When fake Ints" in { any2array(new IntType).apply(List(0.99, true, "-3.8")) shouldEqual Array(0, 1, -3) }
		"When Booleans" in { any2array(new BooleanType).apply(List(true, false, true)) shouldEqual Array(true, false, true) }
		"When fake Booleans" in { any2array(new BooleanType).apply(List(0, "true", new Object)) shouldEqual Array(false, true, true) }
		"When Doubles" in { any2array(new DoubleType).apply(List(2.012d, 0.122d)) shouldEqual Array(2.012d, 0.122d) }
		"When Arrays of num" in { any2array(new ArrayType(new LongType)).apply(List(List(1, 2, 3), List(4, 5, 6))) shouldEqual Array(Array(1, 2, 3), Array(4, 5, 6)) }
		"When from String" in { any2array(new DoubleType).apply("[1,2,3]") shouldEqual Array(1, 2, 3) }
		"When from other" in { any2array(new DoubleType).apply(1000) shouldEqual null }
		"With implicit JsonArrayFormatter" - {
			//			implicit val formatter = new JsonArrayFormatter {}
			"When from String" - {
				"to num" in { any2array(new DoubleType).apply("[1,2,3]") shouldEqual Array(1, 2, 3) }
				"to string" in { any2array(new StringType).apply( """["yeap","nope"]""") shouldEqual Array("yeap", "nope") }
			}
			"When from Bytes" - {
				"to num" in { any2array(new DoubleType).apply("[1,2,3]".getBytes(utils.charset)) shouldEqual Array(1, 2, 3) }
				"to string" in { any2array(new StringType).apply( """["yeap","nope"]""".getBytes) shouldEqual Array("yeap", "nope") }
			}
		}
	}

	"Cast Object" - {
		class Sch1 extends BaseSchema[Sch1] {
			type OBJ = Obj1
			val theName: String = "Sch 1"
			val p0 = Str_prop()("p0")
			val p1 = Boo_prop()("p1")
			val p2 = Lng_prop()("p2", _.text = "ok")
			def newObject: Obj1 = new Obj1

			class Obj1 extends Obj
		}
		val sch1 = new Sch1

		//
		"When self" in {
			val obj = sch1()
			any2object(sch1).apply(obj) shouldEqual obj
		}
		"When null" in { any2object(sch1).apply(null) shouldEqual null }
		"When String" in {
			val obj = sch1().setValuesArray(Array("ok", true, 100))
			any2object(sch1).apply( """["ok",true,100]""") shouldEqual obj
		}
		"With implicit JsonArrayFormatter" - {
			implicit val formatter = new JsonArrayFormatter {}
			"When String" - {
				val obj = sch1().setValuesArray(Array("ok", true, 100))
				"When String" in { any2object(sch1).apply( """["ok",true,100]""") shouldEqual obj }
			}
			"When from Bytes" - {
			}
		}
	}
}
