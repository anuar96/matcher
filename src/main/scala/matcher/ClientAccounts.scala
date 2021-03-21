package matcher

import com.typesafe.scalalogging.StrictLogging
import matcher.model._

class ClientAccounts(clients: Map[String, ClientAccount]) extends StrictLogging {
  def processOrders(orders: Seq[Order]): ClientAccounts = {
    rejectOrders(orders).map {
      case orderBuy: OrderBuy => orders.collectFirst {
        case orderSell: OrderSell if orderSell.clientName == orderBuy.clientName && orderSell.securityType == orderBuy.securityType =>
          matchAndProccessOrders(orders, orderBuy, orderSell)
        case _ => this
      }.getOrElse(this)
      case orderSell: OrderSell =>
        orders.collectFirst {
          case orderBuy: OrderBuy if orderSell.clientName == orderBuy.clientName && orderSell.securityType == orderBuy.securityType =>
            matchAndProccessOrders(orders, orderBuy, orderSell)
          case _ => this
        }.getOrElse(this)
    }
  }

  private def matchAndProccessOrders(orders: Seq[Order], orderBuy: OrderBuy, orderSell: OrderSell) = {
    matchOrders(orderBuy, orderSell) match {
      case Matched(Some(order: OrderBuy), clientAccounts) =>
        val droppedOrderSell = orders.drop(orders.indexOf(orderSell))
        val newOrders = insert(droppedOrderSell, droppedOrderSell.indexOf(orderBuy), order)
        (new ClientAccounts(clientAccounts)).processOrders(newOrders)
      case Matched(Some(orderSell: OrderSell), clientAccounts) =>
        val droppedOrderBuy = orders.drop(orders.indexOf(orderBuy))
        val newOrders = insert(droppedOrderBuy, droppedOrderBuy.indexOf(orderSell), orderSell)
        (new ClientAccounts(clientAccounts)).processOrders(newOrders)
      case Matched(None, clientAccounts) =>
        val droppedOrderBuy = orders.drop(orders.indexOf(orderBuy))
        val newOrders = droppedOrderBuy.drop(droppedOrderBuy.indexOf(orderSell))
        (new ClientAccounts(clientAccounts)).processOrders(newOrders)
      case NonMatched => this
    }
  }

  private def rejectOrders(orders: Seq[Order]): Seq[Order] = {
    orders.flatMap {
      case orderBuy: OrderBuy if clients.get(orderBuy.clientName).exists { clientAcc =>
        clientAcc.cashBalance >= orderBuy.count * orderBuy.cost
      } => Seq(orderBuy)
      case orderBuy: OrderBuy =>
        logger.info(s"${orderBuy.clientName} has not enough cash to execute $orderBuy")
        Nil
      case orderSell: OrderSell if clients.get(orderSell.clientName).exists { clientAcc =>
        orderSell.securityType match {
          case "A" => clientAcc.ABalance >= orderSell.count
          case "B" => clientAcc.BBalance >= orderSell.count
          case "C" => clientAcc.CBalance >= orderSell.count
          case "D" => clientAcc.DBalance >= orderSell.count
        }
      } => Seq(orderSell)
      case orderSell: OrderSell =>
        logger.info(s"${orderSell.clientName} has not enough security balance to execute $orderSell")
        Nil
    }
  }

  sealed trait MatchResult
  case class Matched(newOrder: Option[Order], clientAccounts: Map[String, ClientAccount]) extends MatchResult
  case object NonMatched extends MatchResult


  def matchOrders(orderBuy: OrderBuy, orderSell: OrderSell): MatchResult = {
    if (orderSell.count > orderBuy.count && orderBuy.cost >= orderSell.cost) {
      Matched(Some(orderSell.copy(count = orderSell.count - orderBuy.count)),
        clients.map {
          case (clientName, clientAccount: ClientAccount) if clientName == orderBuy.clientName =>
            clientName -> updateClientAccountBuy(orderSell, clientAccount)
          case (clientName, clientAccount: ClientAccount) if clientName == orderSell.clientName =>
            clientName -> updateClientAccountSell(orderBuy, clientAccount)
          case a => a
        })
    }
    else if (orderSell.count < orderBuy.count) {
      Matched(Some(orderBuy.copy(count = orderBuy.count - orderSell.count)),
      clients.map {
        case (clientName, clientAccount: ClientAccount) if clientName == orderBuy.clientName =>
          clientName -> updateClientAccountBuy(orderSell, clientAccount)
        case (clientName, clientAccount: ClientAccount) if clientName == orderSell.clientName =>
          clientName -> updateClientAccountSell(orderBuy, clientAccount)
        case a => a
      })
    }
    else if (orderBuy.cost < orderSell.cost){
      NonMatched
    }
    else {
      Matched(None, //TODO пересчет)
    }
  }

  private def insert[T](list: Seq[T], i: Int, values: T*) = {
    val (front, back) = list.splitAt(i)
    front ++ values ++ back
  }


  private def updateClientAccountSell(orderBuy: OrderBuy, clientAccount: ClientAccount): ClientAccount = {
    val accountWithNewSecurityBalance = orderBuy.securityType match {
      case "A" => clientAccount.copy(ABalance = clientAccount.ABalance - orderBuy.count)
      case "B" => clientAccount.copy(BBalance = clientAccount.BBalance - orderBuy.count)
      case "C" => clientAccount.copy(CBalance = clientAccount.CBalance - orderBuy.count)
      case "D" => clientAccount.copy(DBalance = clientAccount.DBalance - orderBuy.count)
    }
    val newCashBalance = clientAccount.cashBalance + orderBuy.cost * orderBuy.count
    accountWithNewSecurityBalance.copy(cashBalance = newCashBalance)
  }

  private def updateClientAccountBuy(orderSell: OrderSell, clientAccount: ClientAccount): ClientAccount = {
    val accountWithNewSecurityBalance: ClientAccount = orderSell.securityType match {
      case "A" => clientAccount.copy(ABalance = clientAccount.ABalance + orderSell.count)
      case "B" => clientAccount.copy(BBalance = clientAccount.BBalance + orderSell.count)
      case "C" => clientAccount.copy(CBalance = clientAccount.CBalance + orderSell.count)
      case "D" => clientAccount.copy(DBalance = clientAccount.DBalance + orderSell.count)
    }
    val newCashBalance: BigInt = clientAccount.cashBalance - orderSell.cost * orderSell.count
    accountWithNewSecurityBalance.copy(cashBalance = newCashBalance)
  }
}
