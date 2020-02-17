object OldCoffee {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, amount: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(amount)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
  }
}

class CreditCard {
  def Charge(price: Double): Unit = {
    println("coffee was charged")
  }
}

case class Charge(cc: CreditCard, amount: Double){
  def combine(other: Charge): Charge ={
    if (cc == other.cc){
      Charge(cc, amount + other.amount)
    }
    else
      throw new Exception("Can't combine charges to different cards ")
  }
}

class Coffee {
  val price = 5.0
}
