package matcher

import com.typesafe.scalalogging.StrictLogging
import matcher.model._

sealed trait MatchResult
case class Matched(newOrder: Option[Order], clientAccounts: Map[String, ClientAccount]) extends MatchResult
case object NonMatched extends MatchResult

case class ClientAccounts(clients: Map[String, ClientAccount])

object ClientAccounts extends StrictLogging {
  def processOrders(clientAccounts: ClientAccounts, orders: Seq[Order]): ClientAccounts = {
    rejectOrders(clientAccounts, orders).foldLeft(clientAccounts) {
      case (clientAccounts: ClientAccounts, orderBuy: OrderBuy) => orders.collectFirst {
        case orderSell: OrderSell if orderSell.clientName == orderBuy.clientName && orderSell.securityType == orderBuy.securityType =>
          matchAndProccessOrders(clientAccounts, orders, orderBuy, orderSell)
        case _ => clientAccounts
      }.getOrElse(clientAccounts)
      case (clientAccounts: ClientAccounts, orderSell: OrderSell) => orders.collectFirst {
        case orderBuy: OrderBuy if orderSell.clientName == orderBuy.clientName && orderSell.securityType == orderBuy.securityType =>
          matchAndProccessOrders(clientAccounts,orders, orderBuy, orderSell)
        case _ => clientAccounts
      }.getOrElse(clientAccounts)
    }
  }

  private def matchAndProccessOrders(clientAccounts: ClientAccounts, orders: Seq[Order], orderBuy: OrderBuy, orderSell: OrderSell): ClientAccounts = {
    matchOrders(clientAccounts, orderBuy, orderSell) match {
      case Matched(Some(order: OrderBuy), clientAccounts) =>
        val droppedOrderSell = orders.drop(orders.indexOf(orderSell))
        val newOrders = insert(droppedOrderSell, droppedOrderSell.indexOf(orderBuy), order)
        processOrders(ClientAccounts(clientAccounts), newOrders)
      case Matched(Some(orderSell: OrderSell), clientAccounts) =>
        val droppedOrderBuy = orders.drop(orders.indexOf(orderBuy))
        val newOrders = insert(droppedOrderBuy, droppedOrderBuy.indexOf(orderSell), orderSell)
        processOrders(ClientAccounts(clientAccounts), newOrders)
      case Matched(None, clientAccounts) =>
        val droppedOrderBuy = orders.drop(orders.indexOf(orderBuy))
        val newOrders = droppedOrderBuy.drop(droppedOrderBuy.indexOf(orderSell))
        processOrders(ClientAccounts(clientAccounts), newOrders)
      case NonMatched => clientAccounts
    }
  }

  private def rejectOrders(clientAccounts: ClientAccounts, orders: Seq[Order]): Seq[Order] = {
    orders.flatMap {
      case orderBuy: OrderBuy if clientAccounts.clients.get(orderBuy.clientName).exists { clientAcc =>
        clientAcc.cashBalance >= orderBuy.count * orderBuy.cost
      } => Seq(orderBuy)
      case orderBuy: OrderBuy =>
        logger.info(s"${orderBuy.clientName} has not enough cash to execute $orderBuy")
        Nil
      case orderSell: OrderSell if clientAccounts.clients.get(orderSell.clientName).exists { clientAcc =>
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

  private def matchOrders(clientAccounts: ClientAccounts, orderBuy: OrderBuy, orderSell: OrderSell): MatchResult = {
    if (orderSell.count > orderBuy.count && orderBuy.cost >= orderSell.cost) {
      Matched(Some(orderSell.copy(count = orderSell.count - orderBuy.count)),
        updateClientAccounts(clientAccounts, orderBuy, orderSell))
    }
    else if (orderSell.count < orderBuy.count) {
      Matched(Some(orderBuy.copy(count = orderBuy.count - orderSell.count)),
        updateClientAccounts(clientAccounts, orderBuy, orderSell))
    }
    else if (orderBuy.cost < orderSell.cost) {
      NonMatched
    }
    else {
      Matched(None, updateClientAccounts(clientAccounts, orderBuy, orderSell))
    }
  }

  private def updateClientAccounts(clientAccounts: ClientAccounts, orderBuy: OrderBuy, orderSell: OrderSell) = {
    clientAccounts.clients.map {
      case (clientName, clientAccount: ClientAccount) if clientName == orderBuy.clientName =>
        clientName -> updateClientAccountBuy(orderSell, clientAccount)
      case (clientName, clientAccount: ClientAccount) if clientName == orderSell.clientName =>
        clientName -> updateClientAccountSell(orderBuy, clientAccount)
      case a => a
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
/* {
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
   }*/