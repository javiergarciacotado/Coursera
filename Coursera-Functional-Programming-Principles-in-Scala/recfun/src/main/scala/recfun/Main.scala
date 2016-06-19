package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def iterate(current: List[Char], acc: Int = 0) : Boolean = {
      if (current.isEmpty || acc < 0) acc == 0
      else if (current.head == '(') iterate(current.tail, acc + 1)
      else if (current.head == ')') acc > 0 && iterate(current.tail, acc - 1)
      else iterate(current.tail, acc)
    }
    iterate(chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money > 0 && !coins.isEmpty) countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0
  }
}
