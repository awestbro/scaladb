package scala

import java.text.SimpleDateFormat

import akka.actor.{Props, ActorSystem, Actor}
import Actor._

sealed trait Message
case class Available extends Message
case class Unavailable extends Message
case class GetTable


case class Table(tableName: String, fields: List[Field]) extends Actor{
  
  def receive = available
  
  def available: Receive = {
    case GetTable => sender ! this
  }
  
  def unavailable: Receive = {
    case GetTable => println("Table " + tableName + " Unavailable"); sender ! Unavailable
  }
 
  //***ROW OPERATIONS***

  var rowCollection = new RowCollection
  rowCollection.fields = fields

  def checkRowType(row: Row): Boolean = {
    if (row.data.size != fields.size) throw new NoSuchElementException("Row is not large enough")
    else
      for (i <- 0 to fields.size - 1) {
        if (fields(i).fieldType.getClass == (row.data(i).getClass) == false) {
          false
        }
      }
    true
  }

  def addRow(row: Row) = {
    context become unavailable
    if (checkRowType(row)) rowCollection.addRow(row) else throw new NoSuchElementException("Could not insert Row. Types do not match")
    context become(available)
  }

  def removeRow(row: Row) =  {
    context become unavailable
    rowCollection.removeRow(row)
    context become available
  }
  
  def update(fieldname: String, value: Any) = {
    context become unavailable
    rowCollection = rowCollection.update(fieldname, value)
    context become available
  }
  
  def updateWhere(fieldname: String, value: Any, whereRc: RowCollection) = {
    context become unavailable
    rowCollection = rowCollection.updateWhere(fieldname, value, whereRc)
    context become available
  }

  //***XML OPERATIONS***

  def toXML = {
    var fieldXML = new scala.xml.NodeBuffer
    fields.foreach(f => fieldXML += f.toXML)
    <table><tablename>{ tableName }</tablename>{ fieldXML }<values>{ rowCollection.toXML }</values></table>
  }

  override def toString = {
    var sb = new StringBuilder
    fields.foreach(f => sb.append(f.toString))
    var sb2 = new StringBuilder
    rowCollection.rc.foreach(row => sb2.append(row.toString + "\n"))
    "Table: " + tableName + "\n" + sb + "\n" + sb2 + "\n"
  }
}
