package just4fun.core.xpression.test


import just4fun.core.xpression.{XNode, XBranch}


object TestBoolOps extends App {
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
		(c0 && c1 && c2 && c3, "c0 && c1 && c2 && c3"),
		((c0 && c1 && c2) && c3, "(c0 && c1 && c2) && c3"),
		(c0 && (c1 && c2 && c3), " c0 && (c1 && c2 && c3)"),
		((c0 && c1) && c2 && c3, "(c0 && c1) && c2 && c3"),
		(c0 && (c1 && c2) && c3, "c0 && (c1 && c2) && c3"),
		(c0 && c1 && (c2 && c3), "c0 && c1 && (c2 && c3)"),
		((c0 && c1) && (c2 && c3), "( c0 && c1) && (c2 && c3)"),

		(c0 || c1 && c2 && c3, "c0 || c1 && c2 && c3"),
		((c0 || c1 && c2) && c3, "(c0 || c1 && c2) && c3"),
		(((c0 || c1) && c2) && c3, "((c0 || c1) && c2) && c3"),
		(c0 || (c1 && c2 && c3), "c0 || (c1 && c2 && c3)"),
		((c0 || c1) && c2 && c3, "(c0 || c1) && c2 && c3"),
		(c0 || (c1 && c2) && c3, "c0 || (c1 && c2) && c3"),
		(c0 || c1 && (c2 && c3), "c0 || c1 && (c2 && c3)"),
		((c0 || c1) && (c2 && c3), "( c0 || c1) && (c2 && c3)"),

		(c0 || c1 || c2 && c3, "c0 || c1 || c2 && c3"),
		((c0 || c1 || c2) && c3, "(c0 || c1 || c2) && c3"),
		(c0 || (c1 || c2 && c3), "c0 || (c1 || c2 && c3)"),
		(c0 || ((c1 || c2) && c3), "c0 || ((c1 || c2) && c3)"),
		((c0 || c1) || c2 && c3, "(c0 || c1) || c2 && c3"),
		(c0 || (c1 || c2) && c3, "c0 ||( c1 || c2) && c3"),
		(c0 || c1 || (c2 && c3), "c0 || c1 || (c2 && c3)"),
		((c0 || c1) || (c2 && c3), "(c0 || c1) || (c2 && c3)"),

		(c0 || c1 || c2 || c3, "c0 || c1 || c2 || c3"),
		((c0 || c1 || c2) || c3, "(c0 || c1 || c2) || c3"),
		(c0 || (c1 || c2 || c3), "c0 || (c1 || c2 || c3)"),
		((c0 || c1) || c2 || c3, "(c0 || c1) || c2 || c3"),
		(c0 || (c1 || c2) || c3, "c0 || (c1 || c2) || c3"),
		(c0 || c1 || (c2 || c3), "c0 || c1 || (c2 || c3)"),
		((c0 || c1) || (c2 || c3), "( c0 || c1) || (c2 || c3)"),

		(c0 && c1 || c2 && c3, "c0 && c1 || c2 && c3"),
		((c0 && c1 || c2) && c3, "(c0 && c1 || c2) && c3"),
		((c0 && (c1 || c2)) && c3, "(c0 && (c1 || c2)) && c3"),
		(c0 && (c1 || c2 && c3), "c0 && (c1 || c2 && c3)"),
		(c0 && ((c1 || c2) && c3), "c0 && ((c1 || c2) && c3)"),
		((c0 && c1) || c2 && c3, "(c0 && c1) || c2 && c3"),
		(c0 && (c1 || c2) && c3, "c0 && (c1 || c2) && c3"),
		(c0 && c1 || (c2 && c3), "c0 && c1 || (c2 && c3)"),
		((c0 && c1) || (c2 && c3), "(c0 && c1) || (c2 && c3)"),

		(c0 || c1 && c2 || c3, "c0 || c1 && c2 || c3"),
		((c0 || c1 && c2) || c3, "(c0 || c1 && c2) || c3"),
		(((c0 || c1) && c2) || c3, "((c0 || c1) && c2) || c3"),
		(c0 || (c1 && c2 || c3), "c0 || (c1 && c2 || c3)"),
		(c0 || (c1 && (c2 || c3)), "c0 || (c1 && (c2 || c3))"),
		((c0 || c1) && c2 || c3, "(c0 || c1) && c2 || c3"),
		(c0 || (c1 && c2) || c3, "c0 || (c1 && c2) || c3"),
		(c0 || c1 && (c2 || c3), "c0 || c1 && (c2 || c3)"),
		((c0 || c1) && (c2 || c3), "(c0 || c1) && (c2 || c3)"),

		(c0 && c1 || c2 || c3, "c0 && c1 || c2 || c3"),
		((c0 && c1 || c2) || c3, "(c0 && c1 || c2) || c3"),
		((c0 && (c1 || c2)) || c3, "(c0 && (c1 || c2)) || c3"),
		(c0 && (c1 || c2 || c3), "c0 && (c1 || c2 || c3)"),
		((c0 && c1) || c2 || c3, "(c0 && c1) || c2 || c3"),
		(c0 && (c1 || c2) || c3, "c0 && (c1 || c2) || c3"),
		(c0 && c1 || (c2 || c3), "c0 && c1 || (c2 || c3)"),
		((c0 && c1) || (c2 || c3), "(c0 && c1) || (c2 || c3)"),

		(c0 && c1 && c2 || c3, "c0 && c1 && c2 || c3"),
		((c0 && c1 && c2) || c3, "(c0 && c1 && c2) || c3"),
		(c0 && (c1 && c2 || c3), "c0 &&( c1 && c2 || c3)"),
		(c0 && (c1 && (c2 || c3)), "c0 &&( c1 && (c2 || c3))"),
		((c0 && c1) && c2 || c3, "(c0 && c1) && c2 || c3"),
		(c0 && (c1 && c2) || c3, "c0 && (c1 && c2) || c3"),
		(c0 && c1 && (c2 || c3), "c0 && c1 && (c2 || c3)"),
		((c0 && c1) && (c2 || c3), "(c0 && c1) && (c2 || c3)")
	)


	BoolScript(list)

}




/* SCRIPT */

object BoolScript {
	val compiler = new Compiler(None)
	def apply(list: List[(XNode, String)]) = {
		val s = s"""
		 | def assign(c0: Boolean, c1: Boolean, c2: Boolean, c3: Boolean) = {
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
		 | assign(true, true, true, true)
		 | assign(true, true, true, false)
		 | assign(true, true, false, false)
		 | assign(true, false, false, false)
		 | assign(false, false, false, false)
		 | assign(false, true, true, true)
		 | assign(false, false, true, true)
		 | assign(false, false, false, true)
		 | assign(false, true, false, true)
		 | assign(true, false, true, false)
		 | assign(true, false, false, true)
		 | assign(false, true, true, false)
		 | """.stripMargin
		println(s)
		compiler.eval[Unit](s)
	}
}

