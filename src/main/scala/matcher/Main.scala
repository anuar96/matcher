package matcher

import java.io.{File, PrintWriter, Writer}
import java.nio.file.Paths

import com.typesafe.scalalogging.StrictLogging
import matcher.csv.WithCsvIO
import matcher.model._

object Main extends App with WithCsvIO with StrictLogging {
  val clients: Map[String, ClientAccount] = ClientsParser.parseClients(Paths.get("clients.txt"))
  logger.debug(s"clients $clients")
  val orders = OrdersParser.parserOrders(Paths.get("orders.txt"))
  logger.debug(s"orders $orders")
  val result = ClientAccounts.processOrders(ClientAccounts(clients), orders.toSeq)
  val printWriter: Writer = new PrintWriter(new File("result.txt"), "US-ASCII")
  val appender = csvWriter.build(printWriter)
  result.clients.values.foreach { a =>
    appender.writeRow(a.toCsvRowSeq: _*)
  }
  appender.close()
}