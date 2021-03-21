package matcher.model

object Order {
  //TODO apply
}

sealed abstract case class Order(
                                  clientName: String,
                                  securityType: String,
                                  cost: BigInt,
                                  count: BigInt)

sealed case class OrderBuy(
                            override val clientName: String,
                            override val securityType: String,
                            override val cost: BigInt,
                            override val count: BigInt
                          ) extends Order(clientName, securityType, cost, count)

sealed case class OrderSell(
                             override val clientName: String,
                             override val securityType: String,
                             override val cost: BigInt,
                             override val count: BigInt
                           ) extends Order(clientName, securityType, cost, count)


case class OrderBuyA(
                      override val clientName: String,
                      override val cost: BigInt,
                      override val count: BigInt) extends OrderBuy(clientName, "A", cost, count)

case class OrderBuyB(
                      override val clientName: String,
                      override val cost: BigInt,
                      override val count: BigInt) extends OrderBuy(clientName, "B", cost, count)

case class OrderBuyC(
                      override val clientName: String,
                      override val cost: BigInt,
                      override val count: BigInt) extends OrderBuy(clientName, "C", cost, count)

case class OrderBuyD(
                      override val clientName: String,
                      override val cost: BigInt,
                      override val count: BigInt) extends OrderBuy(clientName, "D", cost, count)

case class OrderSellA(
                       override val clientName: String,
                       override val cost: BigInt,
                       override val count: BigInt) extends OrderSell(clientName, "A", cost, count)

case class OrderSellB(
                       override val clientName: String,
                       override val cost: BigInt,
                       override val count: BigInt) extends OrderSell(clientName, "B", cost, count)

case class OrderSellC(
                       override val clientName: String,
                       override val cost: BigInt,
                       override val count: BigInt) extends OrderSell(clientName, "C", cost, count)

case class OrderSellD(
                       override val clientName: String,
                       override val cost: BigInt,
                       override val count: BigInt) extends OrderSell(clientName, "D", cost, count)