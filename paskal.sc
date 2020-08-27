package week1

object paskal {

  //count (c, r) element in Pascal triangle
  def pascal(c: Int, r: Int): Int = {
    def findEl(pos: Int, el: Int): Int =
      if (pos == c || pos == r - c) el * (r - pos + 1) / pos
      else findEl(pos + 1, el * (r - pos + 1) / pos)
    if (r <= 1 || c == 0 || r == c) 1
    else findEl(1, 1)
  }                                               //> pascal: (c: Int, r: Int)Int

  pascal(14, 20)                                  //> res0: Int = 38760
	
	//verifies the balancing of parentheses in a string
  def balance(chars: List[Char]): Boolean = {
    def findParentheses(x: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty && x == 0) true
      else if (x < 0 || (chars.isEmpty && x != 0)) false
      else {
        val y = if (chars.head == '(') x + 1 else if (chars.head == ')') x - 1 else x
        findParentheses(y, chars.tail)
      }
    }
    findParentheses(0, chars)
  }                                               //> balance: (chars: List[Char])Boolean
	
	//counts how many different ways you can make change
  balance("((**(())))an".toList)                  //> res1: Boolean = true

  def countChange(money: Int, coins: List[Int]): Int = {
    def combs(money: Int, coins: List[Int]): Int =
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else
        combs(money - coins.head, coins) + combs(money, coins.tail)
    combs(money, coins)
  }                                               //> countChange: (money: Int, coins: List[Int])Int
  countChange(1000, List(50,100, 200,500))        //> res2: Int = 49
List(50,100, 200,500).max                         //> res3: Int = 500
List(50,100, 200,500).sum                         //> res4: Int = 850

}