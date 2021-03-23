package matcher.model

import java.nio.file.Paths

import com.typesafe.scalalogging.StrictLogging
import matcher.{ClientAccounts, ClientsParser, OrdersParser}
import org.scalatest.FunSuite

class ClientAccountSpec extends FunSuite with StrictLogging {
  val resourcesDir = "src/test/resources"
  test("client doesn't have enough A balance to sell") {
    val clients: Map[String, ClientAccount] = ClientsParser.parseClients(Paths.get(s"$resourcesDir/test1/clients.txt"))
    val orders = OrdersParser.parserOrders(Paths.get(s"$resourcesDir/test1/orders.txt"))
    val result = ClientAccounts.processOrders(ClientAccounts(clients), orders.toSeq)

    assert(result ==
      ClientAccounts(
        Map("C1" -> ClientAccount("C1", BigInt(1000), BigInt(10), BigInt(5), BigInt(15), BigInt(0)).get,
          "C2" -> ClientAccount("C2", BigInt(2000), BigInt(3), BigInt(35), BigInt(40), BigInt(10)).get
        ))
    )
  }

  test("client solds A for more money than he wanted in order sell") {
    val clients: Map[String, ClientAccount] = ClientsParser.parseClients(Paths.get(s"$resourcesDir/test2/clients.txt"))
    val orders = OrdersParser.parserOrders(Paths.get(s"$resourcesDir/test2/orders.txt"))
    val result = ClientAccounts.processOrders(ClientAccounts(clients), orders.toSeq)

    assert(result ==
      ClientAccounts(
        Map("C1" -> ClientAccount("C1", BigInt(1500), BigInt(10), BigInt(5), BigInt(15), BigInt(0)).get,
          "C2" -> ClientAccount("C2", BigInt(2000), BigInt(3), BigInt(35), BigInt(40), BigInt(10)).get
        ))
    )
  }
}
