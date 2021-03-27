package matcher.model

sealed trait Order {
  def clientName: String

  def securityType: String

  def cost: BigInt

  def count: BigInt
}

sealed case class OrderBuy(
                            override val clientName: String,
                            override val securityType: String,
                            override val cost: BigInt,
                            override val count: BigInt
                          ) extends Order

sealed case class OrderSell(
                             clientName: String,
                             securityType: String,
                             cost: BigInt,
                             count: BigInt
                           ) extends Order
