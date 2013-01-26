import scala.xml._

object xmlws {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(401); 
var xml = <database>
	<table>
		<tablename>bla</tablename>
		<field>
			<fieldname>int</fieldname>
			<fieldtype>Integer</fieldtype>
		</field>
		<field>
			<fieldname>date</fieldname>
			<fieldtype>Date</fieldtype>
		</field>
		<values>
			<row>
				<column>22.0</column>
				<column>Wed Dec 12 00:00:00 EST 2012</column>
			</row>
		</values>
	</table>
</database>;System.out.println("""xml  : scala.xml.Elem = """ + $show(xml ));$skip(43); 
 
 
  
   var newthign  = (xml \\ "table");System.out.println("""newthign  : scala.xml.NodeSeq = """ + $show(newthign ));$skip(657); 
   
 def xmlParser(s: Elem) = {
   	s \\ "table" foreach { (table) =>
   		var fieldList: List[(String, String)] = List()
   		table \\ "field" foreach { (field) =>
   			fieldList = ((field \\ "fieldname").text, (field \\ "fieldtype").text) :: fieldList
   		}
   		var stringy = "define table " + (table \\ "tablename").text + " having fields("
   		fieldList.map(x => stringy += x._1 + ", "+ x._2); stringy += ")"
   		parseDefine(stringy)
   		var stringy2 = "insert ("
   		table \\ "row" foreach { (row) =>
   			var rowList: List[Any] = List()
   			row \\ "column" foreach { (column) =>
   				stringy2 += column.text + ", "
   			}
   		}
    }
 };System.out.println("""xmlParser: (s: scala.xml.Elem)Unit""");$skip(100); 
 
 def parseDefine(some: String) = {
 	var something = new GrammarParser
 	something.parse(some)
 };System.out.println("""parseDefine: (some: String)Unit""")}
	
}