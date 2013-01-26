import scala.util.parsing.combinator._

object realtest extends JavaTokenParsers{import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(125); 
  println("Welcome to the Scala worksheet");$skip(105); 
  def real: Parser[Double] = """(\d+)(\.\d*)?""".r ^^ { _.toDouble } | """(\.\d+)""".r ^^ { _.toDouble };System.out.println("""real: => realtest.Parser[Double]""");$skip(72); val res$0 = 
                                                
  parseAll(real, "66");System.out.println("""res0: realtest.ParseResult[Double] = """ + $show(res$0));$skip(24); val res$1 = 
  parseAll(real, ".66");System.out.println("""res1: realtest.ParseResult[Double] = """ + $show(res$1))}
}