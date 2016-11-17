import scala.annotation.tailrec

object TS {
  sealed trait Ast {
    def append(c: Char, other: List[Ast]): List[Ast]
    def addParam(c: Ast): Ast = ???

    def length: Int
    def toString(maxLength: Int = 40, indent: String = ""): String
  }
  case class Fin(s: String) extends Ast {
    def append(c: Char, other: List[Ast]): List[Ast] =
      if (c == ' ' && s == "") this :: other else
        Fin(s + c.toString) :: other

    def length = s.length
    def toString(maxLength: Int, indent: String): String = indent + s
  }
  case class Method(name: Ast, params: List[Ast]) extends Ast {
    def append(c: Char, other: List[Ast]): List[Ast] = ??? //Fin(c.toString) :: this :: other
    override def addParam(c: Ast): Method = copy(params = params :+ c)
    def length: Int = name.length + params.map(_.length + 2).sum + 1
    def toString(maxLength: Int, indent: String): String = {
      if (length <= maxLength) {
        indent + name.toString(maxLength) + "(" + params.map(_.toString(maxLength)).mkString(", ") + ")"
      } else {
        s"$indent${name.toString(maxLength)}(\n" + params.map(_.toString(maxLength - 1, indent + " ")).mkString(",\n") + s")"
      }
    }
  }
  
  @tailrec
  def transform(syntaxTree: String, current: List[Ast] = List(Fin(""))): Ast = {
    if (syntaxTree == "") current.head else {
      syntaxTree.head match {
        case '('   => transform(syntaxTree.tail, Fin("") :: Method(current.head, List.empty) :: current.tail)
        case ','   => transform(syntaxTree.tail, Fin("") :: current.tail.head.addParam(current.head) :: current.tail.tail)
        case ')'   => transform(syntaxTree.tail, current.tail.head.addParam(current.head) :: current.tail.tail)
        case other => transform(syntaxTree.tail, current.head.append(other, current.tail))
      }
    }
  }
  
  // used to display showRaw strings more nicely
  def formatted(s: String, maxLength: Int = 60): String = transform(s).toString(maxLength)
}

