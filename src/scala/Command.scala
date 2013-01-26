package scala

class Command 


case class Exit extends Command
case class PrintDictionary extends Command
case class Print(table:String) extends Command
case class DeleteTable(name: String) extends Command
case class DeleteTableWhere(name: String, bool: List[Any])
case class Insert extends Command
case class Update extends Command
case class NewTable(name: String) extends Command