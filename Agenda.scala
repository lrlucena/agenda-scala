import java.io.File
import java.io.PrintWriter

case class Agenda(contatos: List[Contato] = Nil) {
  def inserir(contato: Contato) = copy(contatos = contato :: contatos)
  def remover(contato: Contato) = copy(contatos = contatos.filterNot { contato == _ })
  def procurar(s: String): List[Contato] = contatos.filter(_.conteudo.contains(s))
  def pessoas = contatos.map { case p: Pessoa => Some(p) case _ => None }.flatten
  def empresas = contatos.map { case e: Empresa => Some(e) case _ => None }.flatten

  def porCpf(cpf: String): Option[Pessoa] = pessoas.find { cpf == _ }
  def pofCnpj(cnpj: String): Option[Empresa] = empresas.find { cnpj == _ }

  def salvar = {
      def salvarArq(arq: String, lista: List[Contato])(f: Contato => String) = {
        new PrintWriter(new File(arq)) {
          val a = lista.map { f }
          write(a.mkString("\n"))
          close()
        }
      }

    salvarArq("pessoas.txt", pessoas) {
      case p: Pessoa => s"${p.nome}\t${p.email}\t${p.cidade}\t${p.uf}\t${p.cpf}"
    }
    salvarArq("empresas.txt", empresas) {
      case p: Empresa =>
        s"${p.nome}\t${p.email}\t${p.cidade}\t${p.uf}\t${p.cnpj}\t${p.gerente.map { _.cpf }.getOrElse("")}"
    }
  }
}
