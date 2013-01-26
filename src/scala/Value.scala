package scala;

import java.util.Date
import java.text.SimpleDateFormat

case class Value(data: Any) {
  
  def classtype: String = {
    var returned = data.getClass.getSimpleName
    if (returned == "Double") "Real" else if(returned == "String") "Varchar" else returned
  }
  
  def compareTo(other: Any) = {
	if(data.getClass.getSimpleName != other.getClass.getSimpleName) throw new Exception("Comparing two incompatible types") 
	else {
	  classtype match {
	    case "Int" => data.asInstanceOf[Int] compare other.asInstanceOf[Int]
	    case "Real" => data.asInstanceOf[Double] compare other.asInstanceOf[Double]
	    case "Varchar" => data.asInstanceOf[String] compare other.asInstanceOf[String]
	    case "Boolean" => data.asInstanceOf[Boolean] compare other.asInstanceOf[Boolean]
	    case "Date" => data.asInstanceOf[Date] compareTo other.asInstanceOf[Date]
	  }
	} 
  }
  
  
  override def toString: String = {
    classtype match {
      case "Date" => var formatter = new SimpleDateFormat("dd/MM/yyyy"); formatter.format(data.asInstanceOf[Date])
      case _ => data.toString
    }
  }
  
}
//  
//  def compare(other: Value[A]) = value compare other.value
//}
//
//object Value {
//  implicit def intVal(some: Int) = new Value[Int](some) {
//    def compare(other: Int): Int = value compare other
//  }
//  implicit def doubleVal(some: Double) = new Value[Double](some) {
//    def compare(other: Double): Int = value compare other
//  }
//  implicit def stringVal(some: String) = new Value[String](some) {
//    def compare(other: String): Int = value compare other
//  }
//  implicit def boolVal(some: Boolean) = new Value[Boolean](some) {
//    def compare(other: Boolean): Int = value compare other
//  }
//  implicit def dateVal(some: Date) = new Value[Date](some) {
//    def compare(other: Date): Int = value.compareTo(other)
//    
//    override def toString() = {
//      var formatter = new SimpleDateFormat("dd/MM/yyyy")
//      formatter.format(value)
//    }
//  }
////  implicit def anyVal(some: Any) = new Value[Any](some){
////    def compare(other: Any) = {
////      
////    }
////  }
//  
//  var x = new Value(0)
//}
//
////case class IntegerValue(value: Int) {//extends Ordered[IntegerValue] {
////  def compare(other: IntegerValue): Int = if(value < other.value) -1 else if(value > other.value) 1 else 0
////}
////
////case class RealValue(value: Double) {//extends Ordered[RealValue] {
////  def compare(other: RealValue): Int = if(value < other.value) -1 else if(value > other.value) 1 else 0
////}
////
////case class VarcharValue(value: String) {//extends Ordered[VarcharValue] {
////  def compare(other: VarcharValue): Int = if(value < other.value) -1 else if(value > other.value) 1 else 0
////}
////
////case class BooleanValue(value: Boolean) { //extends Ordered[BooleanValue] {
////  def compare(other: BooleanValue): Int = if(value < other.value) -1 else if(value > other.value) 1 else 0
////}
////
////case class DateValue(value: Date){ //extends Ordered[DateValue] {
////  def compare(other: DateValue): Int = value.compareTo(other.value)
////  
////  override def toString() = {
////    var formatter = new SimpleDateFormat("dd/MM/yyyy")
////    formatter.format(value)
////  }
////}
//////}
//////
//////class RealWrapper[SomeVal](data: Double) extends Comparable {
//////	def compareTo(other: Any): Int = {
//////	  var value = other.asInstanceOf[Double]
//////	  if(data < value) -1 else if(data < value) 1 else 0
//////	}
//////}
//////
//////class VarcharWrapper[SomeVal](data: String) extends Comparable {
//////	def compareTo(other: Any): Int = {
//////	  var value = other.asInstanceOf[String]
//////	  if(data < value) -1 else if(data < value) 1 else 0
//////	}
//////}
//////
//////class BooleanWrapper[SomeVal](data: Boolean) extends Comparable {
//////	def compareTo(other: Any): Int = {
//////	  var value = other.asInstanceOf[Boolean]
//////	  if(data < value) -1 else if(data < value) 1 else 0
//////	}
//////}
//////
//////class DateWrapper[SomeVal](data: Date) extends Comparable {
//////	def compareTo(other: Any): Int = {
//////	  var value = other.asInstanceOf[Date]
//////	  data.compareTo(data)
//////	}
//////}
