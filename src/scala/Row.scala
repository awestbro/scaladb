package scala

import java.text.SimpleDateFormat

case class Row(data: List[Any]) {
  
  override def toString = {
    var sb = new StringBuilder
    data.foreach(d =>  
      sb.append(new Value(d) + "\t\t"))
      "" + sb
  }
  
  def same(other: Row): Boolean = {
    if(data.length != other.data.length) throw new Exception("Rows do not match") else
    for(i <- 0 until data.length) {
      if(Value(data(i)).compareTo(other.data(i)) != 0) false
    }
    true
  }
  
  def toXML() = {
    var rowXML = new scala.xml.NodeBuffer
    data.foreach(r => rowXML += <column>{r}</column>)
    rowXML
  }
  
}