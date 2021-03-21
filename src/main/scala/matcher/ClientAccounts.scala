package matcher

import scala.util.{Success, Try}

import matcher.model._

class ClientAccounts(clients: Map[String, ClientAccount]){
  def partialMatching(orderBuy: Order, orderSell: Order): Unit ={

  }

  def matchOrders(orderBuy: OrderBuy, orderSell: OrderSell): (Option[Order], Map[String, ClientAccount])  ={
    if (orderSell.count > orderBuy.count){
//      OrderSell(orderSell.clientName, orderSell.securityType, orderSell.cost, orderSell.count - orderBuy.count) // ???
      (None, clients)
    }
    else if (orderSell.count < orderBuy.count) {
      orderBuy.copy(count = orderBuy.count - orderSell.count)
      clients.map{
        case (clientName, clientAccount: ClientAccount) if clientName == orderBuy.clientName =>
          clientName -> updateClientAccountBuy(orderSell, clientAccount)
        case (clientName, clientAccount: ClientAccount) if clientName == orderSell.clientName =>
          clientName -> updateClientAccountSell(orderBuy, clientAccount)
        case a => a
      }.toMap
    }
    else{
      (None, //TODO пересчет)
    }
  }

  private def updateClientAccountSell(orderBuy: OrderBuy, clientAccount: ClientAccount): ClientAccount = {
    val accountWithNewSecurityBalance = orderBuy.securityType match{
      case "A" => clientAccount.copy(ABalance = clientAccount.ABalance - orderBuy.count)
      case "B" => clientAccount.copy(BBalance = clientAccount.BBalance - orderBuy.count)
      case "C" => clientAccount.copy(CBalance = clientAccount.CBalance - orderBuy.count)
      case "D" => clientAccount.copy(DBalance = clientAccount.DBalance - orderBuy.count)
    }
    val newCashBalance = clientAccount.cashBalance + orderBuy.cost * orderBuy.count
    accountWithNewSecurityBalance.copy(cashBalance = newCashBalance)
  }

  private def updateClientAccountBuy(orderSell: OrderSell, clientAccount: ClientAccount): ClientAccount = {
    val accountWithNewSecurityBalance = orderSell.securityType match{
      case "A" => clientAccount.copy(ABalance = clientAccount.ABalance + orderSell.count)
      case "B" => clientAccount.copy(BBalance = clientAccount.BBalance + orderSell.count)
      case "C" => clientAccount.copy(CBalance = clientAccount.CBalance + orderSell.count)
      case "D" => clientAccount.copy(DBalance = clientAccount.DBalance + orderSell.count)
    }
    val newCashBalance = clientAccount.cashBalance - orderSell.cost * orderSell.count
    accountWithNewSecurityBalance.copy(cashBalance = newCashBalance)
  }

  def processOrders(orders: Map[String, Order]): ClientAccounts ={
    orders.values.map{
      case orderBuy: OrderBuyA =>
        orders.values.collectFirst{
          case orderSellA: OrderSellA if orderSellA.clientName == orderBuy.clientName  =>

        }

      case OrderBuy(clientName, securityType, cost, count) =>
        orders.values.collectFirst{
          case order: Order if order.clientName == clientName && order.securityType == securityType && order.operation == "s" =>
        }
      case OrderSell(clientName, securityType, cost, count) =>
    }
  }
}
