package just4fun.core.xpression.test

import just4fun.core.xpression.XNode


object TestLogicOps extends App{
	import XNode._

	class Tab extends Table[Tab] {
		val c0 = newProp(0, "c0", IntType)
		val c1 = newProp(1, "c1", IntType)
		val c2 = newProp(2, "c2", IntType)
		val c3 = newProp(3, "c3", IntType)
	}
	val tab = new Tab
	import tab._

	val list: List[(XNode, String)] = List(
		(c0 -2+2*2/2 === 0 || c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 ==! 0,
		  "c0-2+2*2/2 == 0 || c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 != 0"),
		((c0-(1+1)*1/1 === 0 || c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0) || c3/2/2*(4-2-2)+4 ==! 0,
		  "(c0-(1+1)*1/1 == 0 || c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0) || c3/2/2*(4-2-2)+4 != 0"),
		(c0 -(2+2)*2/2 === 0 || (c1-1-1+2 <= 0 || c2/2*(2-1-1+2*1) >= 0 || c3/2/2*4-(2-2+4) ==! 0),
		  "c0-(2+2)*2/2 == 0 || (c1-1-1+2 <= 0 || c2/2*(2-1-1+2*1) >= 0 || c3/2/2*4-(2-2+4) != 0)"),
		(c0 -2+2*(2/2) === 0 && c1-1-1+2 <= 0 || c2/2*(2-1-1+2)*1 >= 0 || c3/2/2*4-2-2+4 ==! 0,
		  "c0-2+2*(2/2) == 0 && c1-1-1+2 <= 0 || c2/(2*2-1-1+2)*1 >= 0 || c3/2/2*4-2-2+4 != 0"),
		((c0-2+2*2/2 === 0 && c1-1-1+2 <= 0 || c2/2*(2-1-1+2)*1 >= 0) || c3/(2/2)*4-2-2+4 ==! 0,
		  "(c0-2+2*2/2 == 0 && c1-1-1+2 <= 0 || c2/2*(2-1-1+2)*1 >= 0) || c3/(2/2)*4-2-2+4 != 0"),
		(c0 -2+2*2/2 === 0 && (c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 ==! 0),
		  "c0-2+2*2/2 == 0 && (c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 != 0)"),
		(c0 -2+2*2/2 === 0 && c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 ==! 0,
		  "c0-2+2*2/2 == 0 && c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 != 0"),
		((c0-2+2*2/2 === 0 && c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0) || c3/2/2*4-2-2+4 ==! 0,
		  "(c0-2+2*2/2 == 0 && c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0) || c3/2/2*4-2-2+4 != 0"),
		(c0 -2+2*2/2 === 0 && (c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 ==! 0),
		  "c0-2+2*2/2 == 0 && (c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 != 0)"),
		(c0 -2+2*2/2 === 0 && c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 && c3/2/2*4-2-2+4 ==! 0,
		  "c0-2+2*2/2 == 0 && c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 && c3/2/2*4-2-2+4 != 0"),
		((c0-2+2*2/2 === 0 && c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0) && c3/2/2*4-2-2+4 ==! 0,
		  "(c0-2+2*2/2 == 0 && c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0) && c3/2/2*4-2-2+4 != 0"),
		(c0 -2+2*2/2 === 0 && (c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 && c3/2/2*4-2-2+4 ==! 0),
		  "c0-2+2*2/2 == 0 && (c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 && c3/2/2*4-2-2+4 != 0)"),
		(c0 -2+2*2/2 === 0 || c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0 && c3/2/2*4-2-2+4 ==! 0,
		  "c0-2+2*2/2 == 0 || c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0 && c3/2/2*4-2-2+4 != 0"),
		((c0-2+2*2/2 === 0 || c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0) && c3/2/2*4-2-2+4 ==! 0,
		  "(c0-2+2*2/2 == 0 || c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0) && c3/2/2*4-2-2+4 != 0"),
		(c0 -2+2*2/2 === 0 || (c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0 && c3/2/2*4-2-2+4 ==! 0),
		  "c0-2+2*2/2 == 0 || (c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0 && c3/2/2*4-2-2+4 != 0)"),
		(c0 -2+2*2/2 === 0 && c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 ==! 0,
		  "c0-2+2*2/2 == 0 && c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 != 0"),
		((c0-2+2*2/2 === 0 && c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0) || c3/2/2*4-2-2+4 ==! 0,
		  "(c0-2+2*2/2 == 0 && c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0) || c3/2/2*4-2-2+4 != 0"),
		(c0 -2+2*2/2 === 0 && c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0 && c3/2/2*4-2-2+4 ==! 0,
		  "c0-2+2*2/2 == 0 && c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0 && c3/2/2*4-2-2+4 != 0"),
		((c0-2+2*2/2 === 0 && c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0) && c3/2/2*4-2-2+4 ==! 0,
		  "(c0-2+2*2/2 == 0 && c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0) && c3/2/2*4-2-2+4 != 0"),
		(c0 -2+2*2/2 === 0 && (c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0 && c3/2/2*4-2-2+4 ==! 0),
		  "c0-2+2*2/2 == 0 && (c1-1-1+2 <= 0 || c2/2*2-1-1+2*1 >= 0 && c3/2/2*4-2-2+4 != 0)"),
		(c0 -2+2*2/2 === 0 || c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 ==! 0,
		  "c0-2+2*2/2 == 0 || c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 != 0"),
		((c0-2+2*2/2 === 0 || c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0) || c3/2/2*4-2-2+4 ==! 0,
		  "(c0-2+2*2/2 == 0 || c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0) || c3/2/2*4-2-2+4 != 0"),
		(c0 -2+2*2/2 === 0 || (c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 ==! 0),
		  "c0-2+2*2/2 == 0 || (c1-1-1+2 <= 0 && c2/2*2-1-1+2*1 >= 0 || c3/2/2*4-2-2+4 != 0)")
	)

	LogicScript(list)

}



/* SCRIPT */

object LogicScript {
	val compiler = new Compiler(None)
	def apply(list: List[(XNode, String)]) = {
		val s = s"""
		 | def assign(c0: Double, c1: Double, c2: Double, c3: Double) = {
		 | var src = false
		 | var gen = false
		 | ${
			val sb = new StringBuilder
			list.foreach { case (n, s) =>
				sb.append("\n src=" + s)
				sb.append("\n gen= " + n.toExpression(0, false))
				sb.append("\n if (src!=gen) {\n println(\"" + s + "\")\nprintln(\"" + n.toExpression(0, false) + "\")\nprintln(\"// \"+c0+\", \"+c1+\", \"+c2+\", \"+c3)\nprintln()\n}")
			}
			sb.toString()
		}
		 | }
		 | assign(0, -5, 6, 3)
		 | assign(-100, 10, 6, 3)
		 | assign(1, 1, 1, 1)
		 | assign(-1, -1, -1, -1)
		 | assign(-100000, -100000, -100000, -100000)
		 | assign(-0.1000001, -0.1000002, 0.1000003, 0.1000004)
		 | assign(0, 0, 0, 0)
		 | assign(0, -10, 2.9, -0.3)
		 | """.stripMargin
		println(s)
		compiler.eval[Unit](s)
	}
}
