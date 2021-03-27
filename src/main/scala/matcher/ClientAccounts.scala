package matcher

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

import com.typesafe.scalalogging.StrictLogging
import matcher.model._

sealed trait MatchResult

case class Matched(newOrder: Option[Order], clientAccounts: Map[String, ClientAccount]) extends MatchResult

case object NonMatched extends MatchResult

case class ClientAccounts(clients: Map[String, ClientAccount])

object ClientAccounts extends StrictLogging {
  def processOrders1(clientAccounts: ClientAccounts, orders: Seq[Order]): ClientAccounts = {
    logger.debug(s"proccess orders $orders with clientAccounts $clientAccounts")
    val cleanedOrders = rejectOrders(clientAccounts, orders)
    logger.debug(s"cleaned orders are $cleanedOrders")
    cleanedOrders.toList match {
      case head :: tail if head.isInstanceOf[OrderBuy] =>
        tail.collectFirst {
          case orderSell: OrderSell if /*orderSell.clientName != orderBuy.clientName && */ orderSell.securityType == head.securityType =>
            logger.debug(s"matchAndProccessOrders $head $orderSell")
            val (updatedClientAccounts, updatedOrders) = matchAndProccessOrders(clientAccounts, cleanedOrders, head.asInstanceOf[OrderBuy], orderSell)
            processOrders1(updatedClientAccounts, updatedOrders)
        }.getOrElse {
          logger.debug(s"didn't find orderSell for $head")
          processOrders1(clientAccounts, tail)
        }
      case head :: tail if head.isInstanceOf[OrderSell] =>
        tail.collectFirst {
          case orderBuy: OrderBuy if /*orderSell.clientName != orderBuy.clientName && */ head.securityType == orderBuy.securityType =>
            logger.debug(s"matchAndProccessOrders $orderBuy $head")
            val (updatedClientAccounts, updatedOrders) = matchAndProccessOrders(clientAccounts, cleanedOrders, orderBuy, head.asInstanceOf[OrderSell])
            processOrders1(updatedClientAccounts, updatedOrders)
        }.getOrElse {
          logger.debug(s"didn't find orderSell for $head")
          processOrders1(clientAccounts, tail)
        }
      case head =>
        logger.debug(s"return clientAccounts $cleanedOrders")
        clientAccounts
    }
  }

  @tailrec
  def processOrders(clientAccounts: ClientAccounts, orders: Seq[Order]): ClientAccounts = {
    val cleanedOrders = rejectOrders(clientAccounts, orders)
    val (updatedClientAccounts, newOrders) = cleanedOrders.foldLeft((clientAccounts, cleanedOrders)) {
      case ((clientAccounts: ClientAccounts, newOrders), orderBuy: OrderBuy) =>
        val rest = newOrders.splitAt(orders.indexOf(orderBuy))._2 //TODO
        rest.collectFirst {
          case orderSell: OrderSell if /*orderSell.clientName != orderBuy.clientName && */ orderSell.securityType == orderBuy.securityType =>
            logger.debug(s"matchAndProccessOrders $orderBuy $orderSell")
            matchAndProccessOrders(clientAccounts, newOrders, orderBuy, orderSell)
        }.getOrElse {
          logger.debug(s"didn't find orderSell for $orderBuy")
          (clientAccounts, cleanedOrders)
        }
      case ((clientAccounts: ClientAccounts, newOrders), orderSell: OrderSell) =>
        val rest: Seq[Order] = newOrders.splitAt(orders.indexOf(orderSell))._2 //TODO
        rest.collectFirst {
          case orderBuy: OrderBuy if /*orderSell.clientName != orderBuy.clientName && */ orderSell.securityType == orderBuy.securityType =>
            logger.debug(s"matchAndProccessOrders $orderBuy $orderSell")
            matchAndProccessOrders(clientAccounts, newOrders, orderBuy, orderSell)
        }.getOrElse {
          logger.debug(s"didn't find orderBuy for $orderSell")
          (clientAccounts, cleanedOrders)
        }
    }

    if (cleanedOrders.length != newOrders.length)
      processOrders(updatedClientAccounts, newOrders)
    else
      updatedClientAccounts
  }

  private def matchAndProccessOrders(clientAccounts: ClientAccounts, orders: Seq[Order], orderBuy: OrderBuy, orderSell: OrderSell): (ClientAccounts, Seq[Order]) = {
    matchOrders(clientAccounts, orderBuy, orderSell) match {
      case Matched(Some(newOrderBuy: OrderBuy), clientAccounts) =>
        logger.info(s"partial matching new orderBuy $newOrderBuy orderSell $orderSell, old orderBuy $orderBuy")
        logger.debug(s"orders is $orders")
        val droppedOrderSell = removeFirstOccuranceOfOrder(orders, orderSell)
        logger.debug(s" droppedOrderSell is $droppedOrderSell")
        val newOrders = replaceOrder(droppedOrderSell, orderBuy, newOrderBuy)
        logger.debug(s"newOrders is $newOrders")
        (ClientAccounts(clientAccounts), newOrders)
      case Matched(Some(newOrderSell: OrderSell), clientAccounts) =>
        logger.info(s"partial matching new orderSell $newOrderSell orderBuy $orderBuy, old orderSell $orderSell")
        logger.debug(s"orders are $orders")
        logger.debug(s"index is ${orders.indexOf(orderBuy)}")
        val droppedOrderBuy = removeFirstOccuranceOfOrder(orders, orderBuy)
        logger.debug(s"droppedOrderBuy is $droppedOrderBuy")
        val newOrders = replaceOrder(droppedOrderBuy, orderSell, newOrderSell)
        logger.debug(s"newOrders $newOrders after partial matching new orderSell $newOrderSell orderBuy $orderBuy, old orderSell $orderSell")
        (ClientAccounts(clientAccounts), newOrders)
      case Matched(None, clientAccounts) =>
        logger.debug(s"full match $orderBuy $orderSell")
        val droppedOrderBuy = removeFirstOccuranceOfOrder(orders, orderBuy)
        val droppedOrderSell = removeFirstOccuranceOfOrder(droppedOrderBuy, orderSell)
        (ClientAccounts(clientAccounts), droppedOrderSell)
      case NonMatched =>
        logger.info(s"non matched $orderBuy $orderSell")
        (clientAccounts, orders)
    }
  }

  private def removeFirstOccuranceOfOrder(orders: Seq[Order], orderToDelete: Order): Seq[Order] = {
    val l = ListBuffer(orders: _*)
    l.remove(orders.indexOf(orderToDelete))
    l
  }

  private def replaceOrder(orders: Seq[Order], orderToDelete: Order, orderToInsert: Order): Seq[Order] = {
    val indexToInsert = orders.indexOf(orderToDelete)
    val droppedOrder = removeFirstOccuranceOfOrder(orders, orderToDelete)
    insert(droppedOrder, indexToInsert, orderToInsert)
  }

  private def rejectOrders(clientAccounts: ClientAccounts, orders: Seq[Order]): Seq[Order] = {
    orders.flatMap {
      case orderBuy: OrderBuy if clientAccounts.clients.get(orderBuy.clientName).exists { clientAcc =>
        clientAcc.cashBalance >= orderBuy.count * orderBuy.cost
      } => Seq(orderBuy)
      case orderBuy: OrderBuy =>
        logger.info(s"${
          orderBuy.clientName
        } has not enough cash to execute $orderBuy")
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
        logger.info(s"${
          orderSell.clientName
        } has not enough security balance to execute $orderSell")
        Nil
    }
  }

  private def matchOrders(clientAccounts: ClientAccounts, orderBuy: OrderBuy, orderSell: OrderSell): MatchResult = {
    if (orderSell.count > orderBuy.count && orderBuy.cost >= orderSell.cost) {
      Matched(Some(orderSell.copy(count = orderSell.count - orderBuy.count)),
        updateClientAccounts(clientAccounts, orderBuy, orderSell))
    }
    else if (orderBuy.count > orderSell.count) {
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
        clientName -> updateClientAccountBuy(orderSell, orderBuy, clientAccount)
      case (clientName, clientAccount: ClientAccount) if clientName == orderSell.clientName =>
        clientName -> updateClientAccountSell(orderBuy, orderSell, clientAccount)
      case a => a
    }
  }

  private def insert[T](list: Seq[T], i: Int, values: T*) = {
    val (front, back) = list.splitAt(i)
    front ++ values ++ back
  }


  private def updateClientAccountSell(orderBuy: OrderBuy, orderSell: OrderSell, clientAccount: ClientAccount): ClientAccount = {
    val count = orderSell.count min orderBuy.count

    val accountWithNewSecurityBalance = orderBuy.securityType match {
      case "A" => clientAccount.copy(ABalance = clientAccount.ABalance - count)
      case "B" => clientAccount.copy(BBalance = clientAccount.BBalance - count)
      case "C" => clientAccount.copy(CBalance = clientAccount.CBalance - count)
      case "D" => clientAccount.copy(DBalance = clientAccount.DBalance - count)
    }
    val newCashBalance = clientAccount.cashBalance + orderSell.cost * count //TODO orderSell.cost???
    accountWithNewSecurityBalance.copy(cashBalance = newCashBalance)
  }

  private def updateClientAccountBuy(orderSell: OrderSell, orderBuy: OrderBuy, clientAccount: ClientAccount): ClientAccount = {
    val count = orderSell.count min orderBuy.count

    val accountWithNewSecurityBalance: ClientAccount = orderSell.securityType match {
      case "A" => clientAccount.copy(ABalance = clientAccount.ABalance + count)
      case "B" => clientAccount.copy(BBalance = clientAccount.BBalance + count)
      case "C" => clientAccount.copy(CBalance = clientAccount.CBalance + count)
      case "D" => clientAccount.copy(DBalance = clientAccount.DBalance + count)
    }
    val newCashBalance: BigInt = clientAccount.cashBalance - orderSell.cost * count
    accountWithNewSecurityBalance.copy(cashBalance = newCashBalance)
  }
}
