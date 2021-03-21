package matcher.model

import scala.util.{Failure, Success, Try}

object ClientAccount {
  def apply(clientName: String,
            cashBalance: BigInt,
            ABalance: BigInt,
            BBalance: BigInt,
            CBalance: BigInt,
            DBalance: BigInt): Try[ClientAccount] = {
    if (cashBalance >= 0 && ABalance >= 0 && BBalance >= 0 && CBalance >= 0 && DBalance >= 0)
      Success(new ClientAccount(clientName, cashBalance, ABalance, BBalance, CBalance, DBalance))
    else Failure(new IllegalArgumentException("balances can't be negative"))
  }
}

case class ClientAccount(clientName: String,
                         cashBalance: BigInt,
                         ABalance: BigInt,
                         BBalance: BigInt,
                         CBalance: BigInt,
                         DBalance: BigInt){
  def toCsvRow = s"$clientName $cashBalance $ABalance $BBalance $CBalance $DBalance"
  def toCsvRowSeq = Seq(clientName, cashBalance.toString(), ABalance.toString(), BBalance.toString(), CBalance.toString(), DBalance.toString())
}

