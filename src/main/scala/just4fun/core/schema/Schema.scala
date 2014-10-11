package just4fun.core.schema

import java.lang.reflect.Field

/* IMPLICITS */
object SchemaImplicits {
	implicit def prop2type[T, S <: Schema[S]](prop: Schema[S]#Prop[T]): PropType[T] with BaseType[_] = prop.valueType
}


/** TODO: Two strategies of Prop organization:
  * 1. With auto prop.index.
  * 2. With defined prop.index. */

trait Schema[S <: Schema[S]] {theScheme: S =>
	import SchemaImplicits._
	/* SCHEMA */

	type OBJ <: Obj
	type PROP[t] <: Prop[t]
	val theName: String
	// Override in subclass if required
	val theVersion = 0
	// MAX value = 128 (prop index from 0 to 127)
	var propsSize: Byte = 0
	protected var tmpProps = List[S#PROP[_]]()
	/** Order by id */
	lazy val props: List[S#PROP[_]] = propsAll.filterNot(_ isSTUB).toList
	/** */
	lazy val propsAll: Vector[S#PROP[_]] = {
		var tmp = Vector[S#PROP[_]]().padTo(propsSize, null)
		tmpProps.foreach(p => tmp = tmp.updated(p.index, p))
		val tmp2: IndexedSeq[S#PROP[_]] = for (n <- 0 until propsSize) yield tmp(n) match {
			case null => STUB_prop(n.toByte)(n.toString).asInstanceOf[S#PROP[_]]
			case p => p.asInstanceOf[S#PROP[_]]
		}
		tmpProps = null
		tmp2.toVector
	}
	lazy val propsFields: List[S#PROP[_]] = {
		var list = List[S#PROP[_]]()
		newObject.getClass.getDeclaredFields.foreach { f => props.find(_.linkField(f)).foreach { p => list = p :: list } }
		list
	}
	/** Override in subclasses if required */
	implicit val utils: TypeUtils with Formatter = new TypeUtils with Formatter {}
	//	protected lazy val sampleObj = newObject
	//	lazy val objectType = new ObjectType(this)(utils, manifest[sampleObj.type].asInstanceOf[Manifest[S#OBJ]])

	/* PROPS MANAGEMENT */
	protected def newProp[T: Manifest](index: Byte, name: String, typ: PropType[T] with BaseType[_]): PROP[T]

	protected def constructProp[T: Manifest](index: Byte, name: String, typ: PropType[T] with BaseType[_], initCode: S#PROP[_] => Unit = null): S#PROP[T] = {
		val p = newProp(index, name, typ).asInstanceOf[S#PROP[T]]
		if (initCode != null) initCode(p)
		addProp(p)
		p
	}
	/** Helper methods for Prop creation with specific predefined types. */
	def STUB_prop(index: Byte = propsSize)(name: String, initCode: S#PROP[_] => Unit = null): S#PROP[String] =
		constructProp(index, name, StubType, initCode)
	def Str_prop(index: Byte = propsSize)(name: String, initCode: S#PROP[_] => Unit = null): S#PROP[String] =
		constructProp(index, name, StringType, initCode)
	def Bin_prop(index: Byte = propsSize)(name: String, initCode: S#PROP[_] => Unit = null): S#PROP[Array[Byte]] =
		constructProp(index, name, BinaryType, initCode)
	def Lng_prop(index: Byte = propsSize)(name: String, initCode: S#PROP[_] => Unit = null): S#PROP[Long] =
		constructProp(index, name, LongType, initCode)
	def Int_prop(index: Byte = propsSize)(name: String, initCode: S#PROP[_] => Unit = null): S#PROP[Int] =
		constructProp(index, name, IntType, initCode)
	def Dbl_prop(index: Byte = propsSize)(name: String, initCode: S#PROP[_] => Unit = null): S#PROP[Double] =
		constructProp(index, name, DoubleType, initCode)
	def Flt_prop(index: Byte = propsSize)(name: String, initCode: S#PROP[_] => Unit = null): S#PROP[Float] =
		constructProp(index, name, FloatType, initCode)
	def Boo_prop(index: Byte = propsSize)(name: String, initCode: S#PROP[_] => Unit = null): S#PROP[Boolean] =
		constructProp(index, name, BooleanType, initCode)
	def Obj_prop[T <: Schema[T]](index: Byte = propsSize)(name: String, schema: T, initCode: S#PROP[_] => Unit = null)(implicit m: Manifest[T#OBJ]): S#PROP[T#OBJ] =
		constructProp(index, name, new ObjectType(schema), initCode)
	def Arr_prop[T: Manifest](index: Byte = propsSize)(name: String, elementType: PropType[T] with BaseType[_], initCode: S#PROP[_] => Unit = null): S#PROP[Array[T]] =
		constructProp(index, name, new ArrayType[T](elementType), initCode)


	protected def addProp[T: Manifest](p: S#PROP[T]) = tmpProps.find(_.index == p.index) match {
		case None => tmpProps = p :: tmpProps
			//			println(s"Prop with index ${p.index } is defined ok.")
			if (p.index >= propsSize) propsSize = (p.index + 1).toByte
		case _ => // TODO implicit logger
			println(s"Prop with index ${p.index } is defined. Indexes should be unique.")
	}

	/* OBJECT  CREATION */
	/** Override if object sub-class is required */
	protected def newObject: OBJ

	def apply(): S#OBJ = newObject.asInstanceOf[S#OBJ]

	override def toString: String = propsAll.mkString("[", ", ", "]")







	/* OBJECT */

	trait Obj {this: OBJ =>
		implicit val thisObj: S#OBJ = this.asInstanceOf[S#OBJ]
		val scheme: S = theScheme
		lazy val propsChanged = collection.mutable.Set[S#PROP[_]]()
		protected lazy val _values = new Array[Any](propsSize)
		type ReadValueFn1 = (Int, S#PROP[_], Any) => Unit
		type WriteValueFn1 = (Int, S#PROP[_]) => Any
		type ReadValueFn2 = (S#PROP[_], Any) => Unit
		type WriteValueFn2 = S#PROP[_] => Any
		type PropFilterFn = S#PROP[_] => Boolean

		/** Callbacks. Can be overridden */
		protected def onUpgrade(): Unit = ()
		protected def onLoad(): Unit = ()
		protected def onSave(): Unit = ()



		/* SET VALUES */

		def setValuesAll(write: WriteValueFn1): S#OBJ = setValuesOf(propsAll)(write)
		def setValuesLive(write: WriteValueFn1): S#OBJ = setValuesOf(props)(write)
		/** Writes values iterating over supplied props used as sequence-like iterable. */
		def setValuesOf(props: Iterable[S#PROP[_]])(write: WriteValueFn1): S#OBJ = {
			// DEFs
			var n = 0
			def iterate(): Unit = props.foreach { p => _values(p.index) = write(n, p); n += 1 }
			// EXEC
			setValues(_ => iterate())
		}
		/** Writes values based on external sequence-like iterable over all props. */
		def setValuesIterating(iterate: (WriteValueFn2 => Boolean) => Unit): S#OBJ = {
			// DEFs
			var n = 0
			def onNextProp(write: WriteValueFn2): Boolean = {
				val doNext = n < propsSize
				if (doNext) _values(n) = write(propsAll(n))
				n += 1
				doNext
			}
			// EXEC
			setValues(_ => iterate(onNextProp))
		}
		/** Writes values based on external map-like iterable over arbitrary param of props. */
		def setValuesFinding(iterate: ((PropFilterFn, WriteValueFn2) => Boolean) => Unit): S#OBJ = {
			// DEFs
			def onFindProp(filter: PropFilterFn, write: WriteValueFn2): Boolean =
				props.find(filter) match {
					case Some(p) => _values(p.index) = write(p); true
					case None => false
				}
			// EXEC
			setValues(_ => iterate(onFindProp))
		}
		// TODO replace Iterable to Array ???
		def setValuesArray(iterable: Iterable[_]): S#OBJ = setValues { _ =>
			var n = 0
			iterable.foreach { v => if (n < propsSize) _values(n) = v; n += 1 }
		}
		def setValuesMap(map: collection.Map[String, Any]): S#OBJ = setValues { _ =>
			map.foreach { case (k, v) =>
				props.find(_.name == k).foreach(p => _values(p.index) = v)
			}
		}
		def setValues(writeValues: S#OBJ => Unit): S#OBJ = {
			writeValues(thisObj)
			onUpgrade()
			onLoadInternal()
			thisObj
		}

		/* GET VALUES */

		/** For the change of Prop to be tracked update it explicitly (mutation of reference can't be detected) */
		def getChanges(read: ReadValueFn1): Unit = getValuesOf(propsChanged)(read)
		def getValuesAll(read: ReadValueFn1): Unit = getValuesOf(propsAll)(read)
		def getValuesLive(read: ReadValueFn1): Unit = getValuesOf(props)(read)
		/** Reads values iterating over supplied props used as sequence-like iterable. */
		def getValuesOf(props: Iterable[S#PROP[_]])(read: ReadValueFn1): Unit = {
			// DEFs
			var n = 0
			def iterate(): Unit = props.foreach { p => read(n, p, _values(p.index)); n += 1 }
			// EXEC
			getValues(_ => iterate())
		}
		/** Reads values based on external sequence-like iterable over all props. */
		def getValuesIterating(iterate: (ReadValueFn2 => Boolean) => Unit): Unit = {
			// DEFs
			var n = 0
			def onNextProp(read: ReadValueFn2): Boolean = {
				val doNext = n < propsSize
				if (doNext) read(propsAll(n), _values(n))
				n += 1
				doNext
			}
			// EXEC
			getValues(_ => iterate(onNextProp))
		}
		/** Reads values based on external map-like iterable over arbitrary param of props. */
		def getValuesFinding(iterate: ((PropFilterFn, ReadValueFn2) => Boolean) => Unit): Unit = {
			// DEFs
			def onFindProp(filter: PropFilterFn, read: ReadValueFn2): Boolean =
				props.find(filter) match {
					case Some(p) => read(p, _values(p.index)); true
					case None => false
				}
			// EXEC
			getValues(_ => iterate(onFindProp))
		}
		def getValuesArray: Array[Any] = getValues { _ =>
			val arr = new Array[Any](propsSize)
			propsAll.foreach { p => arr(p.index) = p.asBaseType(_values(p.index)) }
			arr
		}
		def getValuesMap: Map[String, Any] = getValues { _ =>
			var map = Map[String, Any]()
			props.foreach { p => map += (p.name -> p.asBaseType(_values(p.index), false)) }
			map
		}
		def getValues[T](readValues: S#OBJ => T): T = {
			onSaveInternal()
			readValues(thisObj)
		}


		/*  VALUE SET */

		/** NON-TYPE-SAFE value assignment. Does NOT register Prop change NOR update field */
		def setRawValue(prop: S#PROP[_], v: Any): Unit = _values(prop.index) = v
		/** Value assignment with '=' syntax. Registers Prop change */
		def update[T](prop: S#PROP[T], v: T): Unit = {
			_values(prop.index) = v
			propsChanged += prop.asInstanceOf[S#PROP[_]]
			if (prop.field != null) prop.field.set(this, v)
		}
		/** Value assignment with chaining */
		def apply[T](prop: S#PROP[T], v: T): S#OBJ = {
			update[T](prop, v)
			thisObj
		}
		def update(dummy: Nothing): Unit = ()

		/*  VALUE GET */
		/** NON-TYPE-SAFE  value access */
		def getRawValue(prop: S#PROP[_]): Any = _values(prop.index)
		/** Value access */
		def apply[T](prop: S#PROP[T]): T = {
			val v0 = _values(prop.index)
			val v1 = prop.evaluate(v0)
			// refresh converted value (but null to track 'empty' values)
			if (v0 != null && v0 != v1) _values(prop.index) = v1
			v1
		}

		/* MISC */
		/** Checks whether value exists (ie non-null) */
		def hasValue(prop: S#PROP[_]): Boolean = _values(prop.index) != null
		/** Sets value to null and registers change. */
		def clearValue(prop: S#PROP[_]): Unit = if (hasValue(prop)) {
			_values(prop.index) = null
			propsChanged += prop
			if (prop.field != null) prop.field.set(this, prop.asNull)
		}

		def changed = propsChanged.nonEmpty
		def copy(): S#OBJ = getValues { _ =>
			scheme().setValues { obj =>
				Array.copy(_values, 0, obj._values, 0, propsSize)
			}
		}
		override def equals(any: Any): Boolean = any match {
			case obj: Obj => eq(obj) || (scheme.eq(obj.scheme) && _values.sameElements(obj._values))
			case _ => super.equals(any)
		}
		override def toString: String = utils.stringFromObj(scheme)(thisObj)

		/* INTERNALS */

		protected def onLoadInternal() = {
			if (propsFields.nonEmpty) propsFields.foreach { p => p.field.set(this, apply(p)) }
			onLoad()
		}
		protected def onSaveInternal() = {
			if (propsFields.nonEmpty) propsFields.foreach { p =>
				val v0 = _values(p.index)
				val v1 = p.field.get(this)
				if (v0 != v1 && (v0 != null || p.asNull != v1)) {
					_values(p.index) = v1
					propsChanged += p
				}
			}
			onSave()
		}

	}








	/* PROP */

	abstract class Prop[T] {this: PROP[T] =>
		val thisProp: S#PROP[T] = this.asInstanceOf[S#PROP[T]]
		val scheme: S = theScheme
		/** from 0 to 127 */
		val index: Byte
		val name: String
		val valueType: PropType[T] with BaseType[_]
		var field: Field = null
		val isSTUB = valueType.isInstanceOf[StubType]
		lazy val typeName = {
			valueType.mft.runtimeClass.getSimpleName + (valueType.mft.typeArguments match {
				case Nil => ""
				case list => list.map(_.runtimeClass.getSimpleName).mkString("[", ",", "]")
			})
		}

		/** Value assignment */
		def update(v: T)(implicit obj: S#OBJ): Unit = obj.update(thisProp, v)
		/** Value access */
		def apply()(implicit obj: S#OBJ): T = obj.apply(thisProp)

		override def toString: String = s"[$index,$name,$typeName]"

		def linkField(f: Field): Boolean = if (f.getName != name) false
		else if (valueType.mft.runtimeClass == f.getType) {
			// Dangerous code: Lost generic type info due to erasure.
			f.setAccessible(true)
			field = f
			true
		}
		else {
			// TODO implicit logger
			println(s"Can't link prop $name with same-named field due to the type mismatch. Prop type= $typeName; field type= ${f.getType }")
			false
		}

	}








	/* PROP & BASE Type singletons  */

	object StringType extends StringType

	object BinaryType extends BinaryType

	object LongType extends LongType

	object IntType extends IntType

	object DoubleType extends DoubleType

	object FloatType extends FloatType

	object BooleanType extends BooleanType

	object StubType extends StubType

}
