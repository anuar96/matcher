package matcher.model

import java.nio.file.Paths

import com.typesafe.scalalogging.StrictLogging
import matcher.{ClientAccounts, ClientsParser, OrdersParser}
import org.scalatest.FunSuite

class ClientAccountSpec extends FunSuite with StrictLogging {
  val resourcesDir = "src/test/resources"

  def testCase(testDirectory: String): Unit = {
    val clients: Map[String, ClientAccount] = ClientsParser.parseClients(Paths.get(s"$resourcesDir/$testDirectory/clients.txt"))
    val orders = OrdersParser.parserOrders(Paths.get(s"$resourcesDir/$testDirectory/orders.txt"))
    val result: ClientAccounts = ClientAccounts.processOrders(ClientAccounts(clients), orders.toSeq)

    val expectedResult = ClientAccounts(ClientsParser.parseClients(Paths.get(s"$resourcesDir/$testDirectory/result.txt")))

    assert(result == expectedResult)
  }

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

  test("partial matching") {
    testCase("test2")
  }

  test("orders must be executed in right sequence") {
    testCase("test3")
  }

  test("nothing changes") {
    testCase("test4")
  }

  test("partial matching sell") {
    testCase("test5")
  }

  test("partial matching sell and buy") {
    testCase("test6")
  }
}



