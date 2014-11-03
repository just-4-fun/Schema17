package just4fun.core.xpression.test


import just4fun.core.xpression.XNode


object TestMathOps extends App {
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
		(c0 * c1 * c2, "c0*c1*c2"),
		(c0 * (c1 * c2), "c0*(c1*c2)"),
		((c0 * c1) * c2, "(c0*c1)*c2"),
		(c0 * c1 / c2, "c0*c1/c2"),
		((c0 * c1) / c2, "(c0*c1)/c2"),
		(c0 * (c1 / c2), "c0*(c1/c2)"),
		(c0 / c1 / c2, "c0/c1/c2"),
		((c0 / c1) / c2, "(c0/c1)/c2"),
		(c0 / (c1 / c2), "c0/(c1/c2)"),
		(c0 / c1 * c2, "c0/c1*c2"),
		((c0 / c1) * c2, "(c0/c1)*c2"),
		(c0 / (c1 * c2), "c0/(c1*c2)"),
		(c0 + c1 + c2, "c0 + c1 + c2"),
		((c0 + c1) + c2, "(c0 + c1) + c2"),
		(c0 + (c1 + c2), "c0 + (c1 + c2)"),
		(c0 + c1 - c2, "c0 + c1 - c2"),
		((c0 + c1) - c2, "(c0 + c1) - c2"),
		(c0 + (c1 - c2), "c0 + (c1 - c2)"),
		(c0 - c1 + c2, "c0 - c1 + c2"),
		((c0 - c1) + c2, "(c0 - c1) + c2"),
		(c0 - (c1 + c2), "c0 - (c1 + c2)"),
		(c0 - c1 - c2, "c0 - c1 - c2"),
		((c0 - c1) - c2, "(c0 - c1) - c2"),
		(c0 - (c1 - c2), "c0 - (c1 - c2)"),
		(5-c0 + 2 * c1 - c2 / c3 - 2, "5-c0+2*c1-c2/c3-2"),
		(5-(c0 + 2) * c1 - c2 / c3 - 2, "5-(c0+2)*c1-c2/c3-2"),
		(5-(c0 + 2 * c1) - c2 / c3 - 2, "5-(c0+2*c1)-c2/c3-2"),
		(5-(c0 + 2 * c1 - c2) / c3 - 2, "5-(c0+2*c1-c2)/c3-2"),
		(5-(c0 + 2 * c1 - c2 / c3) - 2, "5-(c0+2*c1-c2/c3)-2"),
		(5-c0 + 2 * c1 - c2 / (c3 - 2), "5-c0+2*c1-c2/(c3-2)"),
		(5-c0 + 2 * c1 - (c2 / c3 - 2), "5-c0+2*c1-(c2/c3-2)"),
		(5-c0 + 2 * (c1 - c2 / c3 - 2), "5-c0+2*(c1-c2/c3-2)"),
		(5-c0 + (2 * c1 - c2 / c3 - 2), "5-c0+(2*c1-c2/c3-2)"),
		(5-c0 + (2 * (c1 - c2 / (c3 - 2))), "5-c0 + (2 * (c1 - c2 / (c3 - 2)))"),
		(5-(c0 + 2) * (c1 - c2) / (c3 - 2), "5-(c0+2)*(c1-c2)/(c3-2)"),
		(5-(c0 + 2 * c1) - (c2 / c3 - 2), "5-(c0+2*c1)-(c2/c3-2)"),
		(5-((c0 + 2) * (c1 - c2)) / c3 - 2, "5-((c0+2)*(c1-c2))/c3-2"),
		(5-c0 + 2 * (c1 - c2) / (c3 - 2), "5-c0+2*(c1-c2)/(c3-2)"),
		(5-c0 + (2 * c1 - c2 / c3) - 2, "5-c0+(2*c1-c2/c3)-2"),
		(5-c0 + (2 * (c1 - c2) / c3) - 2, "5-c0+(2*(c1-c2)/c3)-2"),
		(5-c0 - c2 + 2 * c1 * 3 / c3 / c1 - 5 / c2 * c3 * 8 - c2 - c3 + 7, "5-c0-c2+2*c1*3/c3/c1-5/c2*c3*8-c2-c3+7"),
		(5-(c0 - c2 + 2) * c1 * (3 / c3) / (c1 - 5) / c2 * c3 * (8 - c2 - c3) + 7, "5-(c0-c2+2)*c1*(3/c3)/(c1-5)/c2*c3*(8-c2-c3)+7"),
		(5-(c0 - c2) + (2 * c1 * 3 / c3) / c1 - 5 / (c2 * c3 * 8) - c2 - c3 + 7, "5-(c0-c2)+(2*c1*3/c3)/c1-5/(c2*c3*8)-c2-c3+7"),
		(5-c0 - (c2 + 2 * c1 * 3 / (c3 / c1 - 5)) / c2 * c3 * 8 - c2 - (c3 + 7), "5-c0-(c2+2*c1*3/(c3/c1-5))/c2*c3*8-c2-(c3+7)"),
		(5-c0 - c2 + 2 * ((c1 * 3) / c3) / c1 - (5 / (c2 * c3)) * 8 - ((c2 - c3) + 7), "5-c0-c2+2*((c1*3)/c3)/c1-(5/(c2*c3))*8-((c2-c3)+7)"),
		(5-c0 - c2 + (2 * c1) * (3 / c3 / c1 - 5 / c2 * c3) * (8 - c2 - c3 + 7), "5-c0-c2+(2*c1)*(3/c3/c1-5/c2*c3)*(8-c2-c3+7)"),
		(5-((c0 - c2) + 2) * c1 * 3 / (c3 / c1 - (5 / c2)) * c3 * (8 - (c2 - c3) + 7), "5-((c0-c2)+2)*c1*3/(c3/c1-(5/c2))*c3*(8-(c2-c3)+7)"),
		(5-c0 - (c2 + 2 * c1 * (3 / c3 / ((c1 - 5) / c2) * c3) * 8 - c2) - c3 + 7, "5-c0-(c2+2*c1*(3/c3/((c1-5)/c2)*c3)*8-c2)-c3+7"),
		(5-c0 - (c2 + 2) * (c1 * (3 / c3 / (c1 - (5 / c2 * c3 * 8 - c2)) - c3) + 7), "5-c0-(c2+2)*(c1*(3/c3/(c1-(5/c2*c3*8-c2))-c3)+7)"),
		(5-c0 - c2 + (2 * c1 * 3 / c3 / (c1 - 5) / c2 * c3) * 8 - c2 - c3 + 7, "5-c0-c2+(2*c1*3/c3/(c1-5)/c2*c3)*8-c2-c3+7"),
		(5-(c0 - c2 + 2 * c1 * 3) / c3 / c1 - (5 / c2 * (c3 * 8 - c2 - c3 + 7)), "5-(c0-c2+2*c1*3)/c3/c1-(5/c2*(c3*8-c2-c3+7))")
	)

	MathScript(list)

}




/* SCRIPT */

object MathScript {
	val compiler = new Compiler(None)
	def apply(list: List[(XNode, String)]) = {
		val s = s"""
		 | def assign(c0: Double, c1: Double, c2: Double, c3: Double) = {
		 | var src = 0D
		 | var gen = 0D
		 | ${
			val sb = new StringBuilder
			list.foreach { case (n, s) =>
				sb.append("\n try {")
				sb.append("\n src= " + s)
				sb.append("\n gen= " + n.toExpression(0, false))
				sb.append("\n if (src!=gen) {\n   println(\"" + s + "\")\n  println(\"" + n.toExpression(0, false)+ "\")\n  println(\"// \"+c0+\", \"+c1+\", \"+c2+\", \"+c3+\"; src= \"+src+\"; gen= \"+gen)\n  println()\n }")
				sb.append("\n } catch { case e: Exception => println(\"    Exception: \" + e+\".\\n    src=" + s + "; gen=" + n.toExpression(0, false) + ";  \"+c0+\", \"+c1+\", \"+c2+\", \"+c3); println() }")
			}
			sb.toString()
		}
		 | }
		 | assign(-100, 10, 6, 3)
		 | assign(1, 1, 1, 1)
		 | assign(-1, -1, -1, -1)
		 | assign(-100000, -100000, -100000, -100000)
		 | assign(-0.1000001, -0.1000002, 0.1000003, 0.1000004)
		 | // assign(0, 0, 0, 0)
		 | assign(0, -10, 2.9, -0.3)
		 | """.stripMargin
		//		println(s)
		compiler.eval[Unit](s)
	}
}


