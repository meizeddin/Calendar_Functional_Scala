package topics

object Unit01 {

  /* nums is a useful list of numbers to use in the exercises */
  val nums: List[Int] = (1 to 10).toList


  /* Exercise 11 code */

  def incr(x: Int): Int = x + 1

  def decr(x: Int): Int = x - 1

  def dbl(x: Int) = x * 2

  //6) a
  def square(x: Int): Int = x * x

  //6) b
  def half(x: Int): Int = x / 2

  //6) c
  def cube(x: Int): Int = x * x * x

  //6) d
  def negate(x: Int): Int = -1 * x

  //6) e
  def minutes(x: Int): Int = x / 60


  @main def exerise11(): Unit =
    //experimenting with incr
    println(incr(1))
    println(incr(incr(1)))

    //experimenting with decr
    println(decr(decr(4))) // should print 2
    println(decr(incr(4))) // should print 4

    //experimenting with dbl
    println(dbl(4)) // should print 8
    println(dbl(dbl(4))) // should print 16
    println(dbl(incr(decr(3)))) // should print 6

    //experimenting with square/ half/ cube/ negate/ minutes
    println(square(4))
    println(cube(4))
    println(half(4))
    println(negate(4))
    println(minutes(155))
    println(half(negate(cube(2))))


  /* Exercise 12 code */

  def add(x: Int)(y: Int): Int = x + y

  def plus(x: Int, y: Int): Int = x + y

  @main def exercise12(): Unit =
    println(add(2)(5))
    println(add(2)(0))
    println(plus(2, 5))
    println(plus(2, 0))

  @main def exercise12b(): Unit =
    println(nums)
    println(nums.map(incr))
    println(nums.map(add(1)))
    println(nums.map(add(100)))
    println(nums.map(add(99)))
    println(nums.map(decr))

  /* Exercise 13 code */

  def sub(x: Int)(y: Int): Int = x - y

  def mul(x: Int)(y: Int): Int = x * y

  def div(x: Int)(y: Int): Int = x / y

  def divBy(x: Int)(y: Int): Int = y / x

  @main def exercise13(): Unit =
    println(s"sub(8)(5) = ${sub(8)(5)}")
    println(s"sub(5)(8) = ${sub(5)(8)}")
    println(s"mul(9)(7) = ${mul(9)(7)}")
    println(s"div(32)(7) = ${div(32)(7)}")
    println(s"divBy(5)(15) = ${divBy(5)(15)}")
    println(nums.map(mul(2)))
    println(nums.map(mul(10)))
    println(nums.map(sub(0)))
    println(nums.map(div(36)))
    println(nums.map(divBy(2)))

  /* subBy subtracts the first argument from the second */
  def subBy(x: Int)(y: Int): Int = y - x

  /* smaller returns the smaller of the two arguments */
  def smaller(x: Int)(y: Int): Int = if(x < y){x}else{y}

  /* balance returns -1 if x < y; 0 if equal; 1 if x > y */
  def balance(x: Int)(y: Int): Int = if(x<y){-1}else if(x>y){1}else{0}

  @main def exercise13b(): Unit =
    println(s"subBy(8)(5) = ${subBy(8)(5)}")
    println(s"smaller(7)(3) = ${smaller(7)(3)}")
    println(s"balance(9)(5) = ${balance(9)(5)}")
    println(s"balance(2)(5) = ${balance(2)(5)}")
    println(s"balance(5)(5) = ${balance(5)(5)}")
    println(nums.map(subBy(10)))
    println(nums.map(smaller(6)))
    println(nums.map(balance(6)))

  /* Exercise 14 code */

  @main def exercise14(): Unit =
    println(nums.map(new Function1[Int, Int] {
      override def apply(x: Int): Int = incr(x)
    }))
    println(nums.map(x => incr(x)))
    println(nums.map(x => x + 1))
    //eta expansion map(incr) == x=>(incr(x)) // x => f(x) == x = f by eta reduction
    println(nums.map(incr))
    //instance of x=>add(1)(x)
    println(nums.map(add(1)))

  /* Exercise 15 code */

  @main def exercise15(): Unit =
    //equivalent to x => sub(x)(5)
    println(nums.map(sub(_)(5)))
    println(nums.map(sub(5)(_)))
    //equivalent to x => div(x)(2)
    println(nums.map(div(_)(2)))

  @main def exercise15b(): Unit =
    println(nums.map(plus(_, 1)))
    println(nums.map(_ + 1))
    println(nums.map(2 * _))
    println(nums.map(balance(_)(6)))
    //Aside exp arithmetic symbols in scala are methods that can be called in the
    //following way. however, scala lets us write them without the .()
    println(1.+(1))
    println(10./(5))
    //this is also doable with methods like this as well
    println(nums map incr)

    //a
    println(nums.map(2*_+1))
    //b
    println(nums.map(_/2))
    //c
    println(nums.map(_*(-1)))
    println(nums.map(0-_))
    //d
    def mapToBoolSmaller(x: Int)(y: Int): Boolean = if(x < y){true}else{false}
    println(nums.map(mapToBoolSmaller(_)(6)))
    println(nums.map(_<6))
    //e
    println(nums.map(_%2==0))

  /* Exercise 16 code */

  @main def exercise16(): Unit =
    println(nums.map(incr.compose(incr)))
    println(nums.map(add(1).compose(mul(2))))
    println(nums.map(mul(2).compose(add(1))))
    println(nums.map(add(1).andThen(mul(2))))
    println(nums.map(mul(2).andThen(add(1))))

    //1
    println("\n1.\tMultiply each value by two, then add five.")
    println(nums.map(add(5).compose(mul(2))))
    println(nums.map(mul(2).andThen(add(5))))
    //2
    println("\n2.\tAdd one, then multiply by ten.")
    println(nums.map(mul(10).compose(add(1))))
    println(nums.map(add(1).andThen(mul(10))))
    //3
    println("\n3.\tNegate, then add one, then multiply by two.")
    println(nums.map(mul(2).compose(add(1).compose(negate))))
    println(nums.map(negate.andThen(add(1).andThen(mul(2)))))
    //4
    println("\n4.\tAdd five, then multiply by ten, then subtract one.")
    println(nums.map(decr.compose(mul(10).compose(add(5)))))
    println(nums.map(add(5).andThen(mul(10).andThen(decr))))


  /* Exercise 17 code */

  val twice: (Int => Int) => (Int => Int) = f => x => f(f(x))
  /*
   * twice takes two parameters, f then x. f is a function (Int => Int) and x is an Int.
   * The effect of twice is to apply a function twice to its argument: e.g.
   *   twice(dbl)(3)
   * = (f => x => f(f(x)))(dbl)(3)    Definition of twice
   * = (x => dbl(dbl(x)))(3)          Beta reduction
   * = dbl(dbl(3))                    Beta reduction
   * = dbl((x => 2*x)(3))             Definition of dbl
   * = dbl(2*3)                       Beta reduction
   * = dbl(6)                         Arithmetic
   * = (x => 2*x)(6)                  Definition of dbl
   * = 2*6                            Beta reduction
   * = 12                             Arithmetic
   *
   * Another way of writing twice is given below (we've called it twice2).
   * It works because (f compose f)(x) = f(f(x)) for all values of x.
   */
  val twice2: (Int => Int) => (Int => Int) = f => f compose f

  val thrice: (Int => Int) => (Int => Int) =  f => x => f(f(f(x)))
  //applies its 1st arg three times to its 2nd arg: i.e. f(f(f(x)))

  val thrice2: (Int => Int) => (Int => Int) = f => f compose f compose f
  // behaves like thrice but be defined in a similar way to twice2

  val octo: (Int => Int) => (Int => Int) = f => x => f(f(f(f(f(f(f(f(x))))))))
  // applies its 1st arg eight times to its 2nd arg

  val octo2: (Int => Int) => (Int => Int) = f => f compose f compose f compose f compose f compose f compose f compose f
  // behaves like octo but is defined using composition

  //val thrice: (Int => Int) => (Int => Int) = ???
  //val thrice2: (Int => Int) => (Int => Int) = ???
  //val octo: (Int => Int) => (Int => Int) = ???
  //val octo2: (Int => Int) => (Int => Int) = ???

  @main def exercise17(): Unit =
    println(twice(incr)(3))
    println(twice2(dbl)(3))
    println(nums.map(thrice(incr)))
    println(nums.map(thrice2(dbl)))
    println(nums.map(octo(incr)))
    println(nums.map(octo(dbl)))
    println(nums.map(octo2(dbl)))



  /* Exercise 18 code */

  val flip: (Int => Int => Int) => (Int => Int => Int) = f => a => b => f(b)(a)
  /*
   * The purpose of flip is to transform a function f so that instead of calculating
   * f(a)(b) it calculates f(b)(a). Essentially, flip(f) behaves like f only with
   * the two arguments swapped over.  This can be useful when reusing existing
   * functions to define new ones:
   */

  val subtract10: Int => Int = flip(sub)(10)
  /*
   * How does this work? Let's try it with 17:
   *   subtract10(17)
   * = flip(sub)(10)(17)                      Definition of subtract10
   * = (f => a => b => f(b)(a))(sub)(10)(17)  Definition of flip
   * = (a => b => sub(b)(a))(10)(17)          Beta reduction
   * = (b => sub(b)(10))(17)                  Beta reduction
   * = sub(17)(10)                            Beta reduction
   * = (x => y => x-y)(17)(10)                Definition of sub
   * = (y => 17-y)(10)                        Beta reduction
   * = 17-10                                  Beta reduction
   * = 7                                      Arithmetic
   */


  val ex18i: Int = flip(div)(7)(flip(sub)(37)(100))
  /*
   * = flip(div)(7)(sub(100)(37))
   * = flip(div)(7)(63)
   * = div(63)(7)
   * = 9
   */

  val ex18ii: Int = (flip(sub)(2) compose flip(div)(10)) (200)
  /*
   * = (flip(sub)(2))(flip(div)(10)(200))
   * = (flip(sub)(2))(div(200)(10))
   * = (flip(sub)(2))(20)
   * = sub(20)(2)
   * = 18
   */

  val ex18iii: Int = flip(flip(sub))(32)(17)
  /*
   * = flip(sub)(17)(32)
   * = sub(32)(17)
   * = 15
   *
   * Thus flip undoes flip.  In fact (flip compose flip) is the identity function
   * i.e. flip compose flip = id
   * where id = x => x
   * as the next example demonstrates.
   */

  val ex18iv: Int = (flip compose flip) (sub)(99)(44)
  /*
   * = id(sub)(99)(44)
   * = sub(99)(44)
   * = 55
   */

  val ex18v: Int = twice(flip(div)(10))(500)
  /*
   * = (flip(div)(10)) (flip(div(10)) (500))    Definition of twice
   * = (flip(div)(10)) (div(500)(10))
   * = (flip(div)(10)) (50)
   * = div(50)(10)
   * = 5
   */


  @main def exercise18(): Unit =
    println(ex18i)
    println(ex18ii)
    println(ex18iii)
    println(ex18iv)
    println(ex18v)

  /*
  Exercise 19

  To show that (flip compose flip) = identity

  */


}
