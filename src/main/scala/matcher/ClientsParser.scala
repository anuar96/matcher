package matcher

import java.nio.charset.Charset
import java.nio.file.Path

import scala.collection.JavaConverters._

import matcher.csv.WithCsvReader
import matcher.model.ClientAccount

object Clients {
  val CLIENT_NAME = 0
  val CASH_BALANCE = 1
  val A_BALANCE = 2
  val B_BALANCE = 3
  val C_BALANCE = 4
  val D_BALANCE = 5
}

object ClientsParser extends WithCsvReader{
  def parseClients(path: Path): Map[String, ClientAccount] ={
    val clientsParser = csvReader.build(path, Charset.forName("US-ASCII"))
    clientsParser.asScala.map { row =>
      val clientName = row.getField(Clients.CLIENT_NAME)
      clientName -> ClientAccount(
        clientName,
        BigInt(row.getField(Clients.CASH_BALANCE)),
        BigInt(row.getField(Clients.A_BALANCE)),
        BigInt(row.getField(Clients.B_BALANCE)),
        BigInt(row.getField(Clients.C_BALANCE)),
        BigInt(row.getField(Clients.D_BALANCE))
      ).get
    }.toMap
  }
}
