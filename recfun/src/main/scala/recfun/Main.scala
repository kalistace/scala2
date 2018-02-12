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
    if (c == 0 || r == c) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def balance(chars: List[Char], openedParentheses: Integer): Boolean = {
      if (chars.isEmpty)
        if (openedParentheses == 0)
          return true
        else
          return false
      else if (chars.head == '(')
        balance(chars.tail, openedParentheses + 1)
      else if (chars.head == ')')
        openedParentheses > 0 && balance(chars.tail, openedParentheses - 1)
      else
        balance(chars.tail, openedParentheses)
    }

    balance(chars, 0)
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money < 0)
      0
    else if (coins.isEmpty)
      0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    // exhaust all combinations subtracting  |   exhausts all combinations using all other coins only
    // the first coin from the money         |
  }

  /*To count all possibilities, we need to iterate all denominations in coins.
  Using recursion, it is the count of using coins[0] plus the count of not using coins[0],
  and recursively check all the denominations.*/
}
