trait Contato {
  val nome, email, cidade, uf: String
  def campos: List[String] = List(nome, email, cidade, uf)
  def conteudo: String = campos.mkString(" ")
}

case class Pessoa(nome: String, email: String, cidade: String, uf: String,
                  cpf: String) extends Contato {
  override def campos = cpf :: super.campos
}

case class Empresa(nome: String, email: String, cidade: String, uf: String,
                   cnpj: String, gerente: Option[Pessoa]) extends Contato {
  override def campos = gerente.map(_.campos).getOrElse(Nil) ++ (cnpj :: super.campos)
}
