package just4fun.core.xpression.test

import just4fun.core.schema.{BaseType, PropType, Schema}
import just4fun.core.xpression.{BuildContext, ColumnXNode, XNode}

trait Table[S <: Table[S]] extends Schema[S] {this: S =>
	type OBJ = TabObj
	type PROP[t] = Column[t]
	val m: Manifest[Int] = manifest[Int]
	override val theName: String = "Table"

	protected def newProp[T: Manifest](index: Byte, name: String, typ: PropType[T] with BaseType[_]): PROP[T] = new Column[T](index, name, typ)

	override protected def newObject: OBJ = new TabObj

	class TabObj extends Obj {self: OBJ =>}


	class Column[T: Manifest](val index: Byte, val name: String, val valueType: PropType[T] with BaseType[_]) extends Prop[T] with ColumnXNode {
		nodeType = {
			val tp = implicitly[Manifest[T]].toString()
			val ix = tp.lastIndexOf('.')
			(if (ix < 0) tp else tp.substring(ix + 1)).toLowerCase
		}
		override def build(cxt: BuildContext, builder: StringBuilder, parent: Option[XNode]): Unit = {
			builder ++= (if (cxt.nameIsIndex) "x" + index else name)
		}
	}
}
