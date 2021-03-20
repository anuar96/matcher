package matcher.model

object Order{
  //TODO apply
}

case class Order(
                  clientName: String,
                  operation: String, //TODO типизировать
                  securityType: String,
                  cost: BigInt,
                  count: BigInt)

