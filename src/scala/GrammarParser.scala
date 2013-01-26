package scala

import scala.util.parsing.combinator._
import java.util.Date
import java.text.SimpleDateFormat
import scala.RowCollection

//DEMONSTRATION MEETING SATURDAY AT 10

object ParseExpr extends GrammarParser {
    def main(args: Array[String]) {
      Iterator.continually(Console.readLine).takeWhile(_ != "#$2Something nobody will type*&#$*&#$").foreach(line => parse(line))
    }
    
}

case class GrammarParser extends JavaTokenParsers{
	def statement: Parser[Any] = (admin_statement | ddl_statement | dml_statement | query_statement)<~";"
	
	def admin_statement: Parser[Any] = exit | print
	
	def print: Parser[Any] = (dictionary | printtable)
	def dictionary: Parser[Any] = "(?i)PRINT".r ~ "(?i)DICTIONARY".r ^^ {x => PrintDictionary}
	def printtable: Parser[Any] = "(?i)PRINT".r ~> table_name ^^ {x => Print(x)}
	def exit: Parser[Any] = "(?i)EXIT".r ^^ (x => Exit)
	def ddl_statement: Parser[Any] = define_table
	
	def define_table: Parser[Any] = define_table1~define_table2 ^^ {case name~fields => TableDatabase.addTable(name, fields); NewTable(name)}
	def define_table1: Parser[String] = "(?i)DEFINE TABLE".r~>table_name ^^ {case x => x}
	def define_table2: Parser[List[Field]] = "(?i)HAVING FIELDS".r~"("~>extended_field_list<~")"
	
	def dml_statement: Parser[Any] = delete | insert | update
	def delete: Parser[Any] = "(?i)DELETE".r~>table_name~opt(where) ^^ {case name~None => DeleteTable(name) case name~Some(bool) => DeleteTableWhere(name, bool)}
	
	def insert: Parser[Any] = insert1~insert2 ^^ {case list~tablename => TableDatabase.getTable(tablename).addRow(new Row(list)); Insert}
	def insert1: Parser[List[Any]] = "(?i)INSERT".r~"("~>value_list<~")"
	def insert2: Parser[String] = "(?i)INTO".r~>table_name
	
	def update: Parser [Any] = "(?i)UPDATE".r~>table_name~updateSet~updateEquals~opt(where) ^^ {case tablename~fieldname~value1~None => TableDatabase.getTable(tablename).update(fieldname, value1); Update
	  																							case tablename~fieldname~value1~Some(wlist) => var whereRc = TableDatabase.getTable(tablename).rowCollection.where(wlist); TableDatabase.getTable(tablename).updateWhere(fieldname, value1, whereRc); Update}
	def updateSet: Parser[String] = "(?i)SET".r~>field_name
	def updateEquals: Parser[Any] = "="~>value
	
	def query_statement: Parser [RowCollection] = selection | projection | join | intersection | union | minus | sort
	def selection: Parser [RowCollection] = "(?i)SELECT".r~>query_list~opt(where) ^^ {case one~None => one case one~Some(two) => one.where(two)}
	def projection: Parser [RowCollection] = "(?i)PROJECT".r~>query_list~"(?i)OVER".r~field_list ^^ {case x~shit~y => x project y}
	def join: Parser [RowCollection] = "(?i)JOIN".r~>query_list~and ^^ {case x~y => x join y}
    def intersection: Parser [RowCollection] = "(?i)INTERSECT".r~>query_list~and ^^ {case x~y => x intersect y}
	def union: Parser [RowCollection] = "(?i)UNION".r~>query_list~and ^^ {case x~y => x union y}
	def minus: Parser [RowCollection] = "(?i)MINUS".r~>query_list~and ^^ {case x~y => x minus y}
	def sort: Parser [RowCollection] = "(?i)ORDER".r~>query_list~"(?i)BY".r~field_name ^^ {case x~shit~y => x sort y}
	def and: Parser [RowCollection] = "(?i)AND".r~>query_list
	def where: Parser[List[Any]] = "(?i)WHERE".r~>boolean_expression
	
	
	
	def table_name: Parser[String] = """\w+""".r
	def query_list: Parser[RowCollection] = table_name ^^ {x=> TableDatabase.getTable(x).rowCollection}| query_statement 
	def title_string: Parser[Any] = """\w+""".r
	def extended_field_list: Parser[List[Field]] = repsep(field_type_list,",")
	def field_type_list: Parser[Field] = field_name~Type ^^ {case name ~ type1 => Field(name, type1)}
	def field_name: Parser[String] = """\w+""".r
	
	def Type: Parser[Value] = TypeInt | TypeVarchar | TypeReal | TypeBool | TypeDate
	def TypeInt: Parser[Value]= "(?i)INTEGER".r ^^ (x => Value(1))
	def TypeVarchar: Parser[Value] = "(?i)VARCHAR".r ^^ (x => Value(""))
	def TypeReal: Parser[Value] = "(?i)REAL".r ^^ (x => Value(1.11))
	def TypeBool: Parser[Value] = "(?i)BOOLEAN".r ^^ (x => Value(true))
	def TypeDate: Parser[Value] = "(?i)DATE".r ^^ (x => Value(new Date())) 
	
	def field_list: Parser[List[String]] = repsep(field_name, ",")
	def boolean_expression: Parser[List[Any]] = field_name~relop~value ^^ {case x~y~z => List(x,y,z)}
	def value: Parser[Any] =  integer ||| real | date | string_expression | boolean 
	def value_list: Parser[List[Any]] = repsep(value,",")
	def letter: Parser[Any] = "[a-zA-Z]".r
	def digit: Parser[Any] = "[0-9]".r
	def char: Parser[Any] = letter
	
	def integer: Parser[Int] = """\d+""".r ^^ {x => augmentString(x).toInt}
    def real: Parser[Double] = """(\d+)(\.\d*)?""".r ^^ {x=>x.toDouble } | """(\.\d*)""".r ^^ {x => x.toDouble }
	def date: Parser[Date] = """'(\d\d)/(\d\d)/(\d\d\d\d)'""".r ^^ {x => dateWrapper(x)}
	def string: Parser[String] = """[^']+""".r ^^ {x => x}
	def string_expression: Parser[String] = "'"~>string<~"'"
	def boolean: Parser[Boolean] = "(?i)true".r ^^ {x => true} | "(?i)false".r ^^ {x => false}
	
	def relop: Parser[String] = "=" | "!=" | "<" | ">" | "<=" | ">="
	
	def dateWrapper(date: String): Date = {
	  var newDate = date.replaceAll("'", "")
	  var dateFormat = new SimpleDateFormat("dd/MM/yyyy")
	  dateFormat.parse(newDate)
	}
	
	def parse(line: String) = {
	  try{
		//println(parseAll(statement, line))
		parseAll(statement,line).getOrElse(println("Error: Command not found")) match {
		  	case Exit => TableDatabase.writeToFile; System.exit(0);
		  	case Print(name) => println(TableDatabase.getTable(name)) 
		  	case PrintDictionary => println(TableDatabase)
		  	//case Table(name, fields) => TableDatabase.addTable(name, fields)
		  	case Insert => println("Inserted row")
		  	case DeleteTable(name) => TableDatabase.removeTable(name)
		  	case DeleteTableWhere(name,bool) => var tableDeletion = TableDatabase.getTable(name).rowCollection.where(bool); tableDeletion.rc.foreach(r => TableDatabase.getTable(name).removeRow(r))
		  	case t: RowCollection => println(t)
		  	case Update => println("Updated the table")
		  	case NewTable(name) => println("Made a new table with name "+ name)
		  	case _ =>println("Not yet implemented")
	    }
	  } catch {
		    case nse: NoSuchElementException => println(nse.getMessage)
		    case e: Exception => println(e.getMessage)
	  }
	}
	
}