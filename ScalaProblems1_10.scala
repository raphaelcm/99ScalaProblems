object ScalaProblems {
	def main(args: Array[String]) {
		val orderedList = List(1, 2, 3, 4, 5, 6, 7, 8)
		val unorderedList = List(4, 3, 6, 8, 7, 1, 2, 5)
		val palindromeListEven = List(1, 1, 2, 2, 3, 3, 2, 2, 1, 1)
		val palindromeListOdd = List(1, 2, 3, 2, 1)
		val fakePalindromeList = List(1, 2, 3, 2, 2, 1)
		val nestedList = List(1, 
						List(2, 
							List(3, 
								List(4, 5),
							6)),
						7,
						List(8, 
							List(9, 
								List(10, 11)),
							12))
		val repeatsList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
		
		println("P01: last(" + orderedList + ") = " + last(orderedList))
		println("P02: penultimate(" + unorderedList + ") = " + penultimate(unorderedList))
		println("P03: nth(4, " + unorderedList + ") = " + nth(4, unorderedList))
		println("P04: length(" + orderedList + ") = " + length(orderedList))
		println("P05: reverse(" + orderedList + ") = " + reverse(orderedList))
		println("P06a: isPalindrome(" + palindromeListEven + ") = " + isPalindrome(palindromeListEven))
		println("P06b: isPalindrome(" + palindromeListOdd + ") = " + isPalindrome(palindromeListOdd))
		println("P06c: isPalindrome(" + fakePalindromeList + ") = " + isPalindrome(fakePalindromeList))
		println("P07: flatten(" + nestedList + ") = " + flatten(nestedList))
		println("P08: compress(" + repeatsList + ") = " + compress(repeatsList))
		println("P09: pack(" + repeatsList + ") = " + pack(repeatsList))
		println("P10: encode(" + repeatsList + ") = " + encode(repeatsList))
	}
	
	//p01
	def last(in: List[Any]): Any = {
		in.last
	}
	
	//p02
	def penultimate(in: List[Any]): Any = {
		in.dropRight(1).last
	}
	
	//p03
	def nth(k: Int, in: List[Any]): Any = {
		in(k)
	}
	
	//p04
	def length(in: List[Any]): Any = {
		in.length
	}
	
	//p05
	def reverse(in: List[Any]): Any = {
		in.reverse
	}

	//p06	
	def isPalindrome(in: List[Any]): Boolean = {
		in == in.reverse
	}
	
	//p07
	def flatten(in: List[Any]): List[Any] = {
		in flatMap {
			case l: List[_] => flatten(l);
			case e => List(e); }
	}

	//p08
	def compress(in: List[Any]): List[Any] = {
		in match {
			case Nil => Nil
			case h :: tail => List(h :: tail.takeWhile(_ == h))
		}
	}
	
	//p09
	def pack(in: List[Any]): List[Any] = {
		val packed = in match {
			case Nil => return Nil
			case h :: tail => h :: tail.takeWhile(_ == h)
		}
		return packed :: pack(in.drop(packed.length))
	}

	//p10
	def encode(in: List[Any]): List[Any] = {
		val packed = pack(in)
		packed flatMap {
			case Nil => Nil
			case l: List[_] => List((l.length, l.head))
		}
	}


}