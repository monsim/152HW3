//questions 1, 2, 3, 4, 5, 6

object DDS {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  
  
  //1
  def controlLoop[S](state: S, cycle: Int, halt: (S, Int)=> Boolean, update: (S, Int)=>S): S =
  		if (halt(state, cycle)) state
  		else controlLoop(update(state, cycle), cycle + 1, halt, update)
                                                  //> controlLoop: [S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S
                                                  //| , Int) => S)S
  		
  		
  	//environmental example IN CLASS. first define how the population change
  	val birthRate = 0.2                       //> birthRate  : Double = 0.2
  	val deathRate = 0.1                       //> deathRate  : Double = 0.1
  	def updatePop(currentPop: Int, numYears: Int) =  {
  		//println("currentPop: " + currentPop) //side effect
  		(currentPop + birthRate * currentPop - deathRate * currentPop).toInt //rounds off double to int
  	}                                         //> updatePop: (currentPop: Int, numYears: Int)Int
  	//next define when to stop. want to see pop 10 years from now
  	def stopDemo(currentPop: Int, numYears: Int) =
  		numYears == 10                    //> stopDemo: (currentPop: Int, numYears: Int)Boolean
  		
  	//call it
  	controlLoop(20, 0, stopDemo, updatePop)   //> res0: Int = 46
  	
  	
  	
  	
  	
  	
  	
  	//2
  	val amoebaIncrease = 2                    //> amoebaIncrease  : Int = 2
  	val max = 100000                          //> max  : Int = 100000
  	
  	def updateAmoeba(currentPop: Int, week: Int) = {
  		currentPop*amoebaIncrease
  	}                                         //> updateAmoeba: (currentPop: Int, week: Int)Int
  	
  	def stopAmoeba(currentPop: Int, week: Int) = {
  		currentPop > max
  	}                                         //> stopAmoeba: (currentPop: Int, week: Int)Boolean
  	
  	controlLoop(10, 0, stopAmoeba, updateAmoeba)
                                                  //> res1: Int = 163840
  	
  	
  	
  	
  	
  	//3
  	//newton's method viewed as a DDS
  	//gives you approx root. guess = guess - f(guess) / f'(guess)
  	def solve (f: Double => Double): Double = {
  		val delta = 1e-14
  		
  		def goodEnough(guess: Double, numGuesses: Int) =
  			math.abs(f(guess)) <= delta
  	
  		def improve(guess: Double, numGuesses: Int) =
  			guess - (f(guess)/df(guess))
  	
  	
  		def df(x: Double) = (f(x+delta) - f(x))/delta
  	
  		//improves guess until it's good enough
  		controlLoop(1.0, 0, goodEnough, improve)
  	
  	}                                         //> solve: (f: Double => Double)Double
  	
  	
  	
  	
  	//4
  	def sqrt(n: Double) = solve((x:Double) => x * x - n) //lamda expression
                                                  //> sqrt: (n: Double)Double
  	
  	sqrt(100)                                 //> res2: Double = 10.0
  	sqrt(49)                                  //> res3: Double = 7.0
  
  
  
  //5
  def cubeRoot(n: Double) = solve((x:Double) => x * x * x - n)
                                                  //> cubeRoot: (n: Double)Double
  
  cubeRoot(8)                                     //> res4: Double = 2.0
  cubeRoot(125)                                   //> res5: Double = 5.0
  
  //6
  def nthRoot(x: Double, n: Int) = solve((y:Double) => ((scala.math.pow(y, n)) - x))
                                                  //> nthRoot: (x: Double, n: Int)Double
  
  nthRoot(9, 2)                                   //> res6: Double = 3.0
  nthRoot(64,3)                                   //> res7: Double = 4.0
  nthRoot(243,5)                                  //> res8: Double = 3.0
}