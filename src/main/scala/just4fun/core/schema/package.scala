package just4fun.core

/**

Mixed Obj fields and props usage.

(Field refers to [[java.lang.reflect.Field]]. Prop refers to [[just4fun.core.schema.Schema.PROP]]. Obj refers to [[just4fun.core.schema.Schema.OBJ]])
  Schema synchronizes values of object fields and same named props.  If some object prop is intensively used it's preferable to create the same named field with same type.

  Linkage prerequisites:
- prop.name should be equal to field name;
- field can have any access modifier;
- field can be val or var;
- field class should be equal to prop generic type;

Prop-field pairs are synchronised in following ways:
- when one of Obj.setValues.. method is called the onLoadInternal method is invoked and all object field values are assigned with linked prop values;
- any update to prop automatically updates linked field;
- update to field does not update linked prop;
- and finally when one of  Obj.getValues.. is called the onSaveInternal is invoked and all prop values are being reassigned with field values.

  */

package object schema {}
