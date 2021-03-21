package matcher.model

import org.scalatest.FunSuite

class ClientAccountSpec extends FunSuite{
  test("copy method of ClientAccount constructor"){
    val clientAccount = ClientAccount("test", BigInt("123"),BigInt("123"),BigInt("123"),BigInt("123"),BigInt("123")).get
    clientAccount.copy(ABalance = clientAccount.ABalance - BigInt("200"))
  }
}
