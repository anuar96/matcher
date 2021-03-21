package matcher.model

sealed abstract class Order(
                             val clientName: String,
                             val securityType: String,
                             val cost: BigInt,
                             val count: BigInt)

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
