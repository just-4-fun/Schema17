package just4fun.core.xpression


object SqlContext {
	val CODE = 0
	val SQLITE = 1
	val MYSQL = 2
}


case class BuildContext(sqlContext: Int = SqlContext.CODE, nameIsIndex: Boolean = true)


object XNode {
	implicit def num2node[T: Numeric](value: T) = NumConst(value)
	implicit def str2node(value: String) = StrConst(value)
	implicit def bool2node(value: Boolean) = BoolConst(value)
	implicit def null2node(value: Null) = NullConst
}

/* OPERATION CLASSes */
trait XOp extends (Int => String)


case class SimpleXOp(name: String) extends XOp {
	override def apply(sqlContext: Int) = name
}


object XOp {
	/* GENERAL OPS */
	lazy val IS = SimpleXOp("IS")
	lazy val ISNOT = SimpleXOp("IS NOT")
	/* NUMERIC OPS */
	lazy val + = SimpleXOp("+")
	lazy val - = SimpleXOp("-")
	lazy val * = SimpleXOp("*")
	lazy val / = SimpleXOp("/")
	lazy val % = SimpleXOp("%")
	lazy val & = SimpleXOp("&")
	lazy val | = SimpleXOp("|")
	lazy val ^ = SimpleXOp("^")
	lazy val ~ = SimpleXOp("~")
	lazy val >> = SimpleXOp(">>")
	lazy val << = SimpleXOp("<<")
	lazy val > = SimpleXOp(">")
	lazy val >= = SimpleXOp(">=")
	lazy val < = SimpleXOp("<")
	lazy val <= = SimpleXOp("<=")
	lazy val ==! = SimpleXOp("!=")
	lazy val === = new XOp {
		override def apply(sqlContext: Int): String = sqlContext match {
			case SqlContext.CODE => "=="
			case _ => "="
		}
	}
	/* STRING OPS */
	lazy val CONCAT = new XOp {
		override def apply(sqlContext: Int): String = sqlContext match {
			case SqlContext.CODE => "+"
			case _ => "&"
		}
	}
	lazy val LIKE = new XOp {
		override def apply(sqlContext: Int): String = sqlContext match {
			case _ => "LIKE"
		}
	}
	/* BOOLEAN OPS */
	lazy val && = new XOp {
		override def apply(sqlContext: Int): String = sqlContext match {
			case SqlContext.CODE => "&&"
			case _ => "AND"
		}
	}
	lazy val || = new XOp {
		override def apply(sqlContext: Int): String = sqlContext match {
			case SqlContext.CODE => "||"
			case _ => "OR"
		}
	}
}


/*  NODE  */

trait XNode {
	private[this] var _nodeType: String = _
	def nodeType: String = _nodeType
	def nodeType_=(t: String) = _nodeType = t

	def toExpression(sqlContext: Int = 0, nameIsIndex: Boolean = true): String = {
		val builder = new StringBuilder
		build(BuildContext(sqlContext, nameIsIndex), builder)
		builder.toString()
	}
	def build(cxt: BuildContext, builder: StringBuilder, parent: Option[XNode] = None): Unit

	/* GENERAL OPS */
	def ===(node: XNode): BoolXBranch = node match {
		case null => BoolXBranch(this, NullConst, XOp.IS)
		case _ => BoolXBranch(this, node, XOp.===)
	}
	def ==!(node: XNode) = node match {
		case null => BoolXBranch(this, NullConst, XOp.ISNOT)
		case _ => BoolXBranch(this, node, XOp.==!)
	}
	def toNum(typ: String = "INTEGER") = NumCast(this, typ)
	def toStr(typ: String = "TEXT") = StrCast(this, typ)
	def toBool(typ: String = "BOOLEAN") = BoolCast(this, typ)
	def in(values: Any*) = ContainsXNode(this, values)
}


trait NumXNode extends XNode {
	def +(node: NumXNode) = NumXBranch(this, node, XOp.+)
	def -(node: NumXNode) = NumXBranch(this, node, XOp.-)
	def *(node: NumXNode) = NumXBranch(this, node, XOp.*)
	def /(node: NumXNode) = NumXBranch(this, node, XOp./)
	def %(node: NumXNode) = NumXBranch(this, node, XOp.%)
	def &(node: NumXNode) = NumXBranch(this, node, XOp.&)
	def |(node: NumXNode) = NumXBranch(this, node, XOp.|)
	def ^(node: NumXNode) = NumXBranch(this, node, XOp.^)
	def ~(node: NumXNode) = NumXBranch(this, node, XOp.~)
	def >>(node: NumXNode) = NumXBranch(this, node, XOp.>>)
	def <<(node: NumXNode) = NumXBranch(this, node, XOp.<<)
	def >(node: NumXNode) = BoolXBranch(this, node, XOp.>)
	def >=(node: NumXNode) = BoolXBranch(this, node, XOp.>=)
	def <(node: NumXNode) = BoolXBranch(this, node, XOp.<)
	def <=(node: NumXNode) = BoolXBranch(this, node, XOp.<=)
	def unary_- = NumNegXNode(this)
	def between[T: Numeric](low: T, top: T) = BetweenXNode(this, low, top, false)
	def notBetween[T: Numeric](low: T, top: T) = BetweenXNode(this, low, top, true)
}

trait StrXNode extends XNode {
	def +|(node: StrXNode) = ConcatXBranch(this, node)
	def =|(node: StrXNode) = BoolXBranch(this, node, XOp.LIKE)
}

trait BoolXNode extends XNode {
	def &&(node: BoolXNode) = LogicXBranch(this, node, XOp.&&)
	def ||(node: BoolXNode) = LogicXBranch(this, node, XOp.||)
	def unary_! = BoolNegXNode(this)
}



/*  BRANCH  */

trait XBranch extends XNode {
	val n1: XNode
	val n2: XNode
	val op: XOp

	var group: Boolean = false

	op match {
		case XOp.* =>
			n1 match {
				case n: XBranch if n.op != XOp.* && n.op != XOp./ && n.op != XOp.% => n.group = true
				case _ =>
			}
			n2 match {
				case n: XBranch if n.op != XOp.* => n.group = true
				case _ =>
			}
		case XOp./ =>
			n1 match {
				case n: XBranch if n.op != XOp.* && n.op != XOp./ && n.op != XOp.% => n.group = true
				case _ =>
			}
			n2 match {
				case n: XBranch => n.group = true
				case _ =>
			}
		case XOp.% =>
			n1 match {
				case n: XBranch if n.op != XOp.* && n.op != XOp./ && n.op != XOp.% => n.group = true
				case _ =>
			}
			n2 match {
				case n: XBranch => n.group = true
				case _ =>
			}
		case XOp.- =>
			n2 match {
				case n: XBranch if n.op == XOp.+ || n.op == XOp.- => n.group = true
				case _ =>
			}
		case XOp.&& =>
			n1 match {
				case n: XBranch if n.op != XOp.&& && !n.isInstanceOf[BoolXBranch] => n.group = true
				case _ =>
			}
			n2 match {
				case n: XBranch if n.op != XOp.&& && !n.isInstanceOf[BoolXBranch] => n.group = true
				case _ =>
			}
		case _ =>
	}
	if (!isInstanceOf[LogicXBranch]) {
		(if (n1.nodeType != null) Some(n1.nodeType) else if (n2.nodeType != null) Some(n2.nodeType) else None).foreach { t => nodeType = t }
		//		println(s"Ltp= ${n1.nodeType};  Rtp= ${n2.nodeType}")
	}
	//	println(s"new Branch ? ${group} [$op]:  n1=${n1.toExpr()}  ? ${n1.group};  n2=${n2.toExpr()}   ? ${n2.group}")
	//	println(toExpression())

	override def nodeType_=(t: String): Unit = {
		if (n1.nodeType == null) n1.nodeType = t
		if (n2.nodeType == null) n2.nodeType = t
		super.nodeType_=(t)
	}

	override def build(cxt: BuildContext, builder: StringBuilder, parent: Option[XNode]): Unit = {
		if (group) builder ++= "("
		n1.build(cxt, builder, Some(this))
		builder ++= s" ${op(cxt.sqlContext) } "
		n2.build(cxt, builder, Some(this))
		if (group) builder ++= ")"
	}
}

case class NumXBranch(n1: XNode, n2: XNode, op: XOp) extends XBranch with NumXNode
case class StrXBranch(n1: XNode, n2: XNode, op: XOp) extends XBranch with StrXNode
case class BoolXBranch(n1: XNode, n2: XNode, op: XOp) extends XBranch with BoolXNode
case class LogicXBranch(n1: XNode, n2: XNode, op: XOp) extends XBranch with BoolXNode


/* COLUMN */

trait ColumnXNode extends NumXNode with StrXNode with BoolXNode



/*  CONSTANTS  */

trait ConstXNode extends XNode {
	val value: Any
	override def build(cxt: BuildContext, builder: StringBuilder, parent: Option[XNode]): Unit = {
		val string = nodeType == "string"
		if (string) builder ++= "'"
		builder ++= value.toString
		if (string) builder ++= "'"
	}
}
// TODO convert string to num
case class NumConst[T: Numeric](value: T) extends ConstXNode with NumXNode
case class StrConst(value: String) extends ConstXNode with StrXNode
case class BoolConst(value: Boolean) extends ConstXNode with BoolXNode

object NullConst extends XNode {
	override def build(cxt: BuildContext, builder: StringBuilder, parent: Option[XNode]): Unit = {
		builder ++= "NULL"
	}
}


/*  CAST  */

trait CastXNode extends XNode {
	val node: XNode
	val typ: String
	override def build(cxt: BuildContext, builder: StringBuilder, parent: Option[XNode]): Unit = {
		builder ++= "CAST("
		node.build(cxt, builder)
		builder ++= s" TO $typ)"
	}
}
case class NumCast(node: XNode, typ: String = "REAL") extends CastXNode with NumXNode
case class StrCast(node: XNode, typ: String = "TEXT") extends CastXNode with StrXNode
case class BoolCast(node: XNode, typ: String = "BOOLEAN") extends CastXNode with BoolXNode



/*  MISC  */

case class BoolNegXNode(node: XNode) extends BoolXNode {
	override def build(cxt: BuildContext, builder: StringBuilder, parent: Option[XNode]): Unit = node match {
		case n: BetweenXNode[_] => n.not = true; node.build(cxt, builder)
		case n: ContainsXNode => n.not = true; node.build(cxt, builder)
		case _ =>
			if (cxt.sqlContext == 0) builder ++= "!(" else builder ++= "NOT("
			node.build(cxt, builder)
			builder ++= ")"
	}
}
case class NumNegXNode(node: XNode) extends NumXNode {
	override def build(cxt: BuildContext, builder: StringBuilder, parent: Option[XNode]): Unit = {
		val branch = node.isInstanceOf[XBranch]
		builder ++= "-"
		if (branch) builder ++= "("
		node.build(cxt, builder)
		if (branch) builder ++= ")"
	}
}


case class ConcatXBranch(n1: StrXNode, n2: StrXNode) extends XBranch with StrXNode {
	lazy val op = XOp.CONCAT
	override def build(cxt: BuildContext, builder: StringBuilder, parent: Option[XNode]): Unit = cxt.sqlContext match {
		case SqlContext.MYSQL =>
			builder ++= "CONCAT("
			n1.build(cxt, builder)
			builder ++= ", "
			n2.build(cxt, builder)
			builder ++= ")"
		case _ => super.build(cxt, builder, parent)
	}
}


case class ContainsXNode(node: XNode, values: Seq[Any], var not: Boolean = false) extends BoolXNode {
	override def build(cxt: BuildContext, builder: StringBuilder, parent: Option[XNode]): Unit = {
		val string = node.nodeType != null && node.nodeType.endsWith("String")
		val branch = node.isInstanceOf[XBranch]
		if (branch) builder ++= "("
		node.build(cxt, builder)
		if (branch) builder ++= ")"
		if (not) builder ++= " NOT"
		builder ++= " IN("
		var nonFirst = false
		values.foreach { v =>
			if (nonFirst) builder ++= "," else nonFirst = true
			if (string) builder ++= "'"
			builder ++= v.toString
			if (string) builder ++= "'"
		}
		builder ++= ")"
	}
}


case class BetweenXNode[T: Numeric](node: XNode, low: T, top: T, var not: Boolean = false) extends BoolXNode {
	override def build(cxt: BuildContext, builder: StringBuilder, parent: Option[XNode]): Unit = {
		val branch = node.isInstanceOf[XBranch]
		if (branch) builder ++= "("
		node.build(cxt, builder)
		if (branch) builder ++= ")"
		if (not) builder ++= " NOT"
		builder ++= s" BETWEEN $low AND $top"
	}
}


trait WhenXNode extends XNode {
	val caseNode: XNode
	val whenThans: Iterable[(XNode, XNode)]
	val elseNode: XNode
	override def build(cxt: BuildContext, builder: StringBuilder, parent: Option[XNode]): Unit = {
		var condType: String = null
		builder ++= "CASE "
		if (caseNode != null) {
			condType = caseNode.nodeType
			caseNode.build(cxt, builder)
		}
		whenThans.foreach { case(when, than) =>
			builder ++= " WHEN "
			if (when.nodeType == null) when.nodeType = condType
			when.build(cxt, builder)
			builder ++= " THEN "
			than.nodeType = nodeType
			than.build(cxt, builder)
		}
		if (elseNode != null) {
			builder ++= " ELSE "
			elseNode.nodeType = nodeType
			elseNode.build(cxt, builder)
		}
		builder ++= " END"// TODO MySql END CASE
	}
}

case class when_s(caseNode: XNode = null)(_whenThans: (XNode, StrXNode)*)(val elseNode: XNode = null) extends WhenXNode with StrXNode {
	val whenThans = _whenThans
	nodeType = "string"
}
case class when_n(caseNode: XNode = null)(_whenThans: (XNode, NumXNode)*)(val elseNode: XNode = null) extends WhenXNode with NumXNode {
	val whenThans = _whenThans
}
case class when_b(caseNode: XNode = null)(_whenThans: (XNode, BoolXNode)*)(val elseNode: XNode = null) extends WhenXNode with BoolXNode {
	val whenThans = _whenThans
}
