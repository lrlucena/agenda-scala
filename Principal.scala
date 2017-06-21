import io.StdIn.{ readInt, readLine }
import scala.annotation.tailrec

trait Operacoes {
  def pergunte(msg: String) = {
    print(msg + ": ")
    readLine()
  }

  val skip = (a: Agenda) => a

  val opcoes: Map[String, Agenda => Agenda] = Map(
    "Sair" -> skip,
    "Inserir Pessoa" -> inserirPessoa,
    "Inserir Empresa" -> inserirEmpresa
  )

  def inserirPessoa(agenda: Agenda): Agenda = {
    println("Inserir Pessoa")
    val p = List("Nome", "e-mail", "Cidade", "UF", "CPF").map { pergunte }
    agenda.inserir(Pessoa(p(0), p(1), p(2), p(3), p(4)))
  }

  def inserirEmpresa(agenda: Agenda): Agenda = ???
  def listarTodos(agenda: Agenda): Agenda = ???
}

object Principal extends App with Operacoes {

  def numerar[T](a: Iterable[T]) = a.zipWithIndex.toMap map { case (a, b) => (b, a) }

  val ops = numerar(opcoes.values).withDefaultValue { skip }
  val lista = numerar(opcoes.keys).map { case (a, b) => s"$a\t$b" } mkString "\n"

  @tailrec def menu(agenda: Agenda = Agenda()) {
    println("Escolha uma opção:")
    println("==================")
    println(lista)
    val a = readInt
    val novaAgenda = ops(a)(agenda)
    if (a != 0) menu(novaAgenda)
  }

  menu()
}