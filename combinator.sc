// questions 1, 2, 3, 4, 5

object combinator {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  class EException extends Exception
  
  
  def inc(x: Double) = x + 1                      //> inc: (x: Double)Double
	def double(x: Double) = 2 * x             //> double: (x: Double)Double

  
  
  //1
  def comb[T](f: T=>T, g: T=>T): T=>T = {
  		def r(x: T): T = f(g(x))
  		r _
  }                                               //> comb: [T](f: T => T, g: T => T)T => T
	   
	
	
	
	
	//2
	
	def id[T](x: T) = x                       //> id: [T](x: T)T
	
	/*
	def selfIter[T](f: T=>T, n: Int): T=>T = {
		if (n == 0) id(f)
		selfIter(comb(f,f), n-1)
	}
	*/
	
	def selfIter[T](f: T=>T, n: Int): T => T = {
  		if (n == 1) id(f)
  		else	 {
  			
  			comb(f, selfIter(f, n - 1))
  		}
  }                                               //> selfIter: [T](f: T => T, n: Int)T => T
  
	//call selfIter f:inc, n:2, x: 4
	selfIter(inc, 2)(4)                       //> res0: Double = 6.0
	selfIter(double, 3)(3)                    //> res1: Double = 24.0
	
	val f = selfIter(double _, 3)             //> f  : Double => Double = combinator$$$Lambda$9/1468177767@506e6d5e
	f(5)                                      //> res2: Double = 40.0
	

	//3
	def countPass[T](arr: Array[T], f:T=>Boolean) = {
		countP(arr, f, 0, 0)
	
	}                                         //> countPass: [T](arr: Array[T], f: T => Boolean)Int
	
	def countP[T](arr: Array[T], f: T=>Boolean, occurences: Int, index: Int): Int = {
		if (arr.length == index) occurences
		else {
			val value = arr(index)
			if(f(value)) countP(arr, f, occurences+1, index+1)
			else countP(arr, f, occurences, index+1)
		}
	}                                         //> countP: [T](arr: Array[T], f: T => Boolean, occurences: Int, index: Int)Int
                                                  //| 
	
	countPass(Array(2, 3, 4, 5), (x: Int) => x % 2 == 0)
                                                  //> res3: Int = 2
  countPass(Array("mom", "rotator", "boy", "racecar"), (x: String) => x == x.reverse)
                                                  //> res4: Int = 3
	
	
	
	//4
	//baseVal: initial value of result
	//combiner: count,result => new result (?)
	//returns a function that maps integers to integers
	
	/* iterative solution
	def makeIter(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int = {
		def f(n: Int): Int = {
			var result = baseVal
			for(count <- 1 to n)
				result = combiner(result, count)
			result
		}
		f _  //returns function
	}
	*/
	
	def recur(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int = {
		def f(n: Int): Int = {
			if (n == 0) baseVal
			else combiner(n, f(n-1))
		}
		f _  //returns function
	}                                         //> recur: (baseVal: Int, combiner: (Int, Int) => Int)Int => Int
	
	
	
	val fact = recur(1, _ * _)                //> fact  : Int => Int = combinator$$$Lambda$15/127618319@2ed94a8b
	fact(5)                                   //> res5: Int = 120
	fact(4)                                   //> res6: Int = 24
	
	val tri = recur(0, (n: Int, m: Int) => n + m)
                                                  //> tri  : Int => Int = combinator$$$Lambda$15/127618319@dfd3711
	tri(5)                                    //> res7: Int = 15
	tri(4)                                    //> res8: Int = 10
	
	
	/*
	//iterative implementation of factorial
	def f(n: Int) = {
		var result = 1		//diff: initial result
		for (count <- 1 to n)
			result = count * result		//diff: how to combine to get new result
		result
	
	}
	*/
	
	
	
	//5
	def deOptionize[T,S](f: T=>Option[S]): T=>S = {
		def r(x: T): S = {
			f(x) match {
				case None => throw new Exception("bad input")
				case Some(result) => result
			}
		}
		r _
	}                                         //> deOptionize: [T, S](f: T => Option[S])T => S
	
	
	def parseDigits(digits: String): Option[Int] =
   if (digits.matches("[0-9]*")) Some(digits.toInt) else None
                                                  //> parseDigits: (digits: String)Option[Int]

	val f1 = deOptionize(parseDigits)         //> f1  : String => Int = combinator$$$Lambda$18/649734728@5f2050f6
	try {
		println(f1("101010"))
		println(f1("a"))
	} catch {
		case e: Exception => println(e)
	}                                         //> 101010
                                                  //| java.lang.Exception: bad input
                       
                       
	
	/*
	def sqrt(x: Double): Option[Double] =
		if (x < 0) None else Some(math.sqrt(x))
		
	
	sqrt(100)
	sqrt(-100)
	
	def sqrt2(x: Double): Double = {
		sqrt(x) match {
			case None => throw new Exception("bad input")
			case Some(result) => result
			}
	}
	
	try {
		println(sqrt2(100))
		println(sqrt2(-100))
	} catch {
		case e: Exception => println(e)
	}
*/
	
}