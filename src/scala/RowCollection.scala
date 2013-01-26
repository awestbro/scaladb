package scala

import java.util.Date

case class RowCollection {
  
	var rc: List[Row] = List()
	
	var fields: List[Field] = List()
	
	override def toString = {
	    var sb = new StringBuilder
	    fields.foreach(f => sb.append(f.toString))
	    var sb2 = new StringBuilder
	    rc.foreach(row => sb2.append(row.toString + "\n"))
	    "RowCollection:\n" + sb + "\n" + sb2 + "\n"
	}
  
    def addRow(row: Row) = rc = row :: rc
	
	def removeRow(row: Row) = rc = rc diff List(row)
	
	def isCompatible(rc1: RowCollection, rc2: RowCollection): Boolean = {
	  if(rc1.rc(0).data.length != rc2.rc(0).data.length) false else
	  for(i <- 0 to rc1.rc.length-1) {
	    for(j <- 0 to rc1.rc(0).data.length-1) {
	    	if(rc1.rc(i).data(j).getClass != rc1.rc(i).data(j).getClass) false
	    }
	  }
	  true
	}
	
	def getColNum(fieldname: String): Int = {
	  var index = fields.indexWhere(f => f.name == fieldname)
	  if(index == -1) throw new Exception("Field with name " + fieldname + " not found") else index
	}
	
	def where(expression:List[Any]): RowCollection = {
	  var toBeReturned = new RowCollection
	  toBeReturned.fields = fields
	  var fieldName = expression(0)
	  var relop = expression(1)
	  var value = expression(2)
	  var valtype = value.getClass.getSimpleName()
	  var index = getColNum(fieldName.toString)
	  var compareList = rc.map(r => Value(r.data(index))compareTo(value))
	  relop match {
	    case "=" => for(i <- 0 until rc.length){if(compareList(i) == 0) toBeReturned.addRow(rc(i))}
	    case "!=" => for(i <- 0 until rc.length){if(compareList(i) != 0) toBeReturned.addRow(rc(i))}
	    case "<" => for(i <- 0 until rc.length){if(compareList(i) < 0) toBeReturned.addRow(rc(i))}
	    case ">" => for(i <- 0 until rc.length){if(compareList(i) > 0) toBeReturned.addRow(rc(i))}
	    case "<=" => for(i <- 0 until rc.length){if(compareList(i) <= 0) toBeReturned.addRow(rc(i))}
	    case ">=" => for(i <- 0 until rc.length){if(compareList(i) >= 0) toBeReturned.addRow(rc(i))}
	  }
	  toBeReturned
	}
	
	def update(fieldname: String, value: Any): RowCollection = {
	  var toBeReturned = new RowCollection
	  var index = getColNum(fieldname)
	  for(i <- 0 until rc.length) {
	    toBeReturned.addRow(Row(rc(i).data.updated(index, value)))
	  }
	  toBeReturned.fields = fields
	  toBeReturned
	}
	
	def updateWhere(fieldname: String, value: Any, whereRc: RowCollection): RowCollection = {
	  var toBeReturned = new RowCollection
	  toBeReturned.rc = rc diff whereRc.rc
	  var newWhere = whereRc.update(fieldname, value)
	  toBeReturned.rc = newWhere.rc ::: toBeReturned.rc
	  toBeReturned.fields = fields
	  toBeReturned
	}
	
	def project(cfields: List[String]): RowCollection = {
	  var toBeReturned = new RowCollection
	  var matching = cfields.map(s => getColNum(s))
	  for(i <- 0 until rc.length) {
	    var newRow: List[Any] = List()
	    for(j <- 0 until matching.length) {
	      newRow = rc(i).data(matching(j)) :: newRow
	    }
	    toBeReturned.addRow(Row(newRow))
	  }
	  var newfields: List[Field] = List()
	  for(i <- 0 until matching.length) {
	    newfields =  fields(matching(i)) :: newfields
	  }
	  toBeReturned.fields = newfields
	  toBeReturned
	}
	
	///NEEDS IMPLEMENTATION
	def sort(fname: String): RowCollection = {
	  var toBeReturned = new RowCollection
	  var index = getColNum(fname)
	  var tupleList: List[(Any, Int)] = List()
	  for(i <- 0 until rc.length){
	    tupleList = (rc(i).data(index), i) :: tupleList
	  }
	  //tupleList = tupleList.sorted
	  tupleList = sortList(tupleList, tupleList(0) _1)
	  tupleList.foreach(tup => toBeReturned.addRow(rc(tup _2)))
	  toBeReturned.fields = fields
	  toBeReturned
	}
	
	def sortList(some: List[(Any,Int)], someType: Any) = {
	   someType.getClass.getSimpleName match {
	     case "Int" => some.asInstanceOf[List[(Int,Int)]].sorted
	     case "Double" => some.asInstanceOf[List[(Double,Int)]].sorted
	     case "String" => some.asInstanceOf[List[(String,Int)]].sorted
	     case "Boolean" => some.asInstanceOf[List[(Boolean,Int)]].sorted
	     case "Date" => some.asInstanceOf[List[(Date,Int)]].sorted
	   }
	}
	
	def union(rc1: RowCollection): RowCollection = {
	  if(isCompatible(this, rc1)) {
		  var toBeReturned = new RowCollection
		  toBeReturned.rc = rc
		  rc1.rc.foreach(r => if(toBeReturned.rc.contains(r)==false) toBeReturned.addRow(r))
		  toBeReturned.fields = fields
		  toBeReturned
	  } else throw new Exception("Two tables not compatible")
	}
	
	def minus(rc1: RowCollection): RowCollection = {
	  if(isCompatible(this, rc1)) {
		  var toBeReturned = new RowCollection
		  toBeReturned.rc = rc
		  rc1.rc.foreach(r => if(toBeReturned.rc.contains(r)==true) toBeReturned.removeRow(r))
		  toBeReturned.fields = fields
		  toBeReturned
	  } else throw new Exception("Two tables not compatible")
	}
	
	def intersect(rc1: RowCollection): RowCollection = {
	  if(isCompatible(this, rc1)) {
		  var toBeReturned = new RowCollection
		  rc1.rc.foreach(r => if(rc.contains(r)) toBeReturned.addRow(r))
		  toBeReturned.fields = fields
		  toBeReturned
	  } else throw new Exception("Two tables not compatible")
	}
	
	def join(rc1: RowCollection): RowCollection = {
	  var toBeReturned = new RowCollection
	  var rowList = for(i <- 0 until rc.length) {
	    for(j <- 0 until rc1.rc.length) {
	      toBeReturned.addRow(Row(rc(i).data :: rc1.rc(j).data)) 
	    }
	  }
	  toBeReturned.fields = fields ::: rc1.fields
	  toBeReturned
	}
	
	def toXML() = {
	  var rowXML = new scala.xml.NodeBuffer
	  rc.foreach{r => rowXML += <row>{r.toXML}</row> }
	  rowXML
	}
	

}