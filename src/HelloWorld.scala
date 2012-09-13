import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input._
import scala.io.Source
import scala.collection.mutable

/**
(def readlineRec (foo) (if foo (cons foo (readline)) nil))
(def readline (readlineRec (read)))
(def printlist (list) (if list (print (car list)) nil) (if list (printlist (cdr list)) nil))
(def greet (who) (printlist (cons 'H' (cons 'e' (cons 'l' (cons 'l' (cons 'o' (cons ' ' who))))))))
(greet (readline))
 */
trait HelloWorldParser extends Parsers {
  override type Elem = Char
  type Eval = Parser[Env => Any]
  
  val primitiveVars = collection.Map("nil" -> (() => Nil))
  val primitiveFunctions = Set(
		Function("cons", env => env[Any]("a1") :: env[List[_]]("a2"), Seq("a1", "a2")),
		Function("car", env => env[Traversable[_]]("a").head, Seq("a")),
		Function("cdr", env => env[Traversable[_]]("a").tail, Seq("a")),
		Function("if", env => env[Any]("a" + (env[Any]("a1") match {case Nil => 3; case x => 2})), Seq("a1", "a2", "a3")),
		Function("read", env => System.in.read() match {case -1 | '\n' => Nil; case x => x.toChar}),
		Function("print", env => print(env("a")), Seq("a")))
  
  case class Function(name: String, body: Env => Any, argNames: Seq[String] = Seq())
  case class Env(vars: mutable.Map[String,() => Any] = mutable.Map() ++ primitiveVars, 
  							 funcs: mutable.Set[Function] = mutable.Set() ++ primitiveFunctions) {
  	def apply[T](varName: String): T = vars(varName)().asInstanceOf[T]
  }
  
  def ws         = elem(' ').+
  def parse      = phrase(prog ~ '\n'           ^^ {case x ~ _ => x})
  def prog: Eval = rep1sep(stmt | expr, ws)     ^^ {case x => env => x map {_(env)} last}
  def stmt       = define                       ^^ {case x => env => x(env)}
  def expr: Eval = (apply | literal | variable) ^^ {case x => env => x(env)}
  
  def variable: Eval = text               ^^ {case name => env => env.vars(name)()}
  def literal: Eval = (int | char)        ^^ {case x => env => x}
  def int = '-'.? ~ elem("", _.isDigit).+ ^^ {case neg ~ value => Integer.parseInt(neg.getOrElse("") + value.mkString)}
  def char = ''' ~> elem("", _ => true) <~ '''     ^^ {case x => x}
  def text = elem("", c => !c.isWhitespace && c != '(' && c != ')').+ ^^ {case x => x.mkString}
  
  def define: Eval = '(' ~ 'd' ~ 'e' ~ 'f' ~ ws ~> text ~ (ws ~ '(' ~ rep1sep(text, ws) ~ ')').? ~ ws ~ prog <~ ')' ^^ {
  	case name ~ args ~ _ ~ body =>
  		env => env.funcs += Function(name, env => body(env), args match {
	  		case None => Seq()
	  		case Some(_ ~ _ ~ args ~ _) => args
  		})
	}
  def apply: Eval = '(' ~> text ~ (ws ~ rep1sep(expr, ws)).? <~ ')' ^^ {
  	case name ~ args => env => {
	  	val actualArgs = args match {
	  		case None => Seq()
	  		case Some(_ ~ args) => args.map(a => {
	  			val v = new {lazy val get = a(env)}
	  			() => v.get
	  		})
	  	}
	  	val func = env.funcs.find(f => f.name == name && f.argNames.size == actualArgs.size).get
	  	func.body( Env(env.vars ++ func.argNames.zip(actualArgs), env.funcs) )
  	}
  }
}
object HelloWorld extends HelloWorldParser {
	def main(args: Array[String]): Unit = {
		val env = Env()
		Source.stdin.getLines.map(_ + '\n').map(new CharSequenceReader(_)).map(parse).foreach { _.get.apply(env) }
	}
}
