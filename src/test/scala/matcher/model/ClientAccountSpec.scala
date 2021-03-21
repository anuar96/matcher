package matcher.model

import com.typesafe.scalalogging.StrictLogging
import org.scalatest.FunSuite

class ClientAccountSpec extends FunSuite with StrictLogging{
  test("copy method of ClientAccount constructor"){
    val clientAccount = ClientAccount("test", BigInt("123"),BigInt("123"),BigInt("123"),BigInt("123"),BigInt("123")).get
    val newClientAcc = clientAccount.copy(ABalance = clientAccount.ABalance - BigInt("200"))
    logger.debug(s"$newClientAcc")
  }
}
