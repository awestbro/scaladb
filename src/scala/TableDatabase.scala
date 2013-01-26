package scala

import scala.xml.XML

import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import akka.dispatch.Await
import akka.util.duration._

object TableDatabase {
  
	val system = ActorSystem()  
	//var tableList: List[Table] = List()
	
	var tableActorList: List[(ActorRef, String)] = List()
  
	def addTable(name: String, fields: List[Field]) = {
	  try{
	  val newTable = system.actorOf(Props(new Table(name, fields)), name = name)
	  tableActorList = (newTable, name) :: tableActorList
	  } catch { case e: InvalidActorNameException => println("Table with that name already exists")}
	}
	
	def removeTable(name: String) = {
		var actorRef = system.stop(system.actorFor(name))
		var actorRefTuple = tableActorList.filter(t => t._2 == name)
		system.stop(actorRefTuple(0)._1)
		tableActorList = tableActorList diff actorRefTuple
	  }
	
	def getTable(name: String): Table = {
	  implicit val timeout = Timeout(5 seconds)
	  var actorRefTuple = tableActorList.filter(t => t._2 == name)
	  var actorRef = actorRefTuple(0)._1
	  
	  var future = actorRef.ask(GetTable)(5)
	  var result = Await.result(future, timeout.duration)
	  
	  while(result.getClass.getSimpleName() != "Table") {
	    future = actorRef.ask(GetTable)(5)
	    result = Await.result(future, timeout.duration)
	  }
	  
	  result.asInstanceOf[Table]
	}
	
	override def toString = {
	  var sb = new StringBuilder
	  tableActorList.foreach{t => sb.append(getTable(t._2).toString)}
	  "\n Table Dictionary: \n\n" + sb
	}
	
	def toXML = {
	  var tableXML = new scala.xml.NodeBuffer 
	  tableActorList.foreach(tbl => tableXML += getTable(tbl._2).toXML) 
	  <database>{tableXML}</database>
	}
	
	def writeToFile = scala.xml.XML.save("database.xml", toXML)
	
	def load = {
	  val data = XML.load("database.xml") 
	}
	
}