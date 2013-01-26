package scala
import scala.Value

case class Field(name: String, fieldType: Value) {
  
//	def getName(): String = {
//	  var name = fieldType.getClass.getSimpleName
//	  if(name == "Double") "Real" else if(name == "String") "Varchar" else name
//	}
  
    override def toString = name + "[" + fieldType.classtype + "]  \t"
    
    def toXML = <field><fieldname>{name}</fieldname><fieldtype>{fieldType.classtype}</fieldtype></field>
}
//p: Person[t] forSome { type t <: Person[t] })