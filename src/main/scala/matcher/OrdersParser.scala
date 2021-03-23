package matcher

import java.nio.charset.Charset
import java.nio.file.{Path, Paths}

import scala.collection.JavaConverters._

import matcher.csv.WithCsvReader
import matcher.model.{Order, OrderBuy, OrderSell}

object Orders {
  val CLIENT_NAME = 0
  val OPERATION = 1
  val SECURITY_NAME = 2
  val cost = 3
  val count = 4
}

object OrdersParser extends WithCsvReader{
  def parserOrders(path: Path): Iterable[Order] ={
    val ordersParser = csvReader.build(path,
      Charset.forName("US-ASCII"))
    ordersParser.asScala.map { row =>
      row.getField(Orders.OPERATION) match {
        case "b" => OrderBuy(row.getField(Clients.CLIENT_NAME), row.getField(Orders.SECURITY_NAME), BigInt(row.getField(Orders.cost)), BigInt(row.getField(Orders.count)))
        case "s" => OrderSell(row.getField(Clients.CLIENT_NAME), row.getField(Orders.SECURITY_NAME), BigInt(row.getField(Orders.cost)), BigInt(row.getField(Orders.count)))
      }
    }
  }
}
