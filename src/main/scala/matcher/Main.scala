package matcher

import java.nio.charset.Charset
import java.nio.file.Paths

import scala.util.Try

import com.typesafe.scalalogging.StrictLogging
import matcher.csv.WithCsvReader
import matcher.model._

object Clients {
  val CLIENT_NAME = 0
  val CASH_BALANCE = 1
  val A_BALANCE = 2
  val B_BALANCE = 3
  val C_BALANCE = 4
  val D_BALANCE = 5
}

object Orders {
  val CLIENT_NAME = 0
  val OPERATION = 1
  val SECURITY_NAME = 2
  val cost = 3
  val count = 4
}

object Main extends App with WithCsvReader with StrictLogging {

  val clientsParser = csvReader.parse(Paths.get("clients.txt"),
    Charset.forName("US-ASCII"))

  val clients: Map[String, Try[ClientAccount]] = Stream.continually(clientsParser.nextRow()).takeWhile(_ != null).map { row =>
    val clientName = row.getField(Clients.CLIENT_NAME)
    clientName -> ClientAccount(
      clientName,
      BigInt(row.getField(Clients.CASH_BALANCE)),
      BigInt(row.getField(Clients.A_BALANCE)),
      BigInt(row.getField(Clients.B_BALANCE)),
      BigInt(row.getField(Clients.C_BALANCE)),
      BigInt(row.getField(Clients.D_BALANCE))
    )
  }.toMap

  logger.debug(s"clients $clients")

  val ordersParser = csvReader.parse(Paths.get("orders.txt"),
    Charset.forName("US-ASCII"))

  val orders: Map[String, Order] = Stream.continually(ordersParser.nextRow()).takeWhile(_ != null).map { row =>
    val clientName = row.getField(Clients.CLIENT_NAME)

    val order: Order = row.getField(Orders.OPERATION) match {
      case "b" => OrderBuy(clientName, row.getField(Orders.SECURITY_NAME), BigInt(row.getField(Orders.cost)), BigInt(row.getField(Orders.count)))
      case "s" => OrderSell(clientName, row.getField(Orders.SECURITY_NAME), BigInt(row.getField(Orders.cost)), BigInt(row.getField(Orders.count)))
    }
    clientName -> order
  }.toMap



  logger.debug(s"orders $orders")
}

