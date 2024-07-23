package topics
import lib.logic.L3.*

object Unit07 {

  /*
   * Exercise 71
  */

  @main def exercise71(): Unit =
    val f: Formula = Cond(A, B)
    val env: Map[TruthVar, TruthVal] = Map(A -> U, B -> F)
    println(f)
    println(f.showTruthTable)
    println(f.evaluate(env))

    val f1: Formula = Eqv(And(A, B),And(B, A))
    val f1Env: Map[TruthVar, TruthVal] = Map(A -> F, B -> T)
    println(f1)
    println(f1.showTruthTable)
    println(f1.evaluate(f1Env))

    val f2: Formula = Eqv(And(A, Or(B, C) ), Or(And(A, B), And(A, C)))
    val f2Env: Map[TruthVar, TruthVal] = Map(A -> U, B -> T, C -> F)
    println(f2)
    println(f2.showTruthTable)
    println(f2.evaluate(f2Env))

    val f3: Formula = Cond(A, Cond(Not(A), A))
    val f3Env: Map[TruthVar, TruthVal] = Map()
    println(f3)
    println(f3.showTruthTable)
    println(f3.evaluate(f3Env))

    val f4: Formula = Or(A, Not(A))
    val f4Env: Map[TruthVar, TruthVal] = Map(A -> U)
    println(f4)
    println(f4.showTruthTable)
    println(f4.evaluate(f4Env))

    val f5: Formula = L(Cond(Not(A), U))
    val f5Env: Map[TruthVar, TruthVal] = Map(A -> T)
    println(f5)
    println(f5.showTruthTable)
    println(f5.evaluate(f5Env))

    val f6: Formula = Eqv(Cond(A, B), Cond(Not(B), Not(A)))
    val f6Env: Map[TruthVar, TruthVal] = Map(A -> F)
    println(f6)
    println(f6.showTruthTable)
    println(f6.evaluate(f6Env))

  /*
   * Exercise 72
  */
  @main def exercise72(): Unit =
    val f: Formula = Cond(A, Eqv(B, And(C, Or(D, Not(Cond(E, F))))))
    println(f)
    lazy val g: Formula = f map {
      case A|B|C => T
      case D => D
      case E => E
      case F => F
      case U => U
      case T => T
    }
    println(g)

  /*
   * Exercise 73
  */
  def equiv(f1: Formula, f2: Formula): Boolean = {
    //val vars1: Seq[TruthVar] = f1.getVars
    //val vars2: Seq[TruthVar] = f2.getVars
    //val combinations1: Seq[Seq[TruthVal]] = L3.truthValCombinations(vars1.length)
    //val combinations2: Seq[Seq[TruthVal]] = L3.truthValCombinations(vars2.length)
    //val results1 = for row <- combinations yield this.evaluate((vars1 zip row).toMap)
    //val results2 = for row <- combinations yield this.evaluate((vars2 zip row).toMap)
    val f: Formula = Eqv(f1, f2)
    //f.generateTruthTable._3.forall(_==T)
    f.isTautology
  }

  @main def exercise73(): Unit =
    println(equiv(A, Not(Not(A))))
    println(equiv(And(A, B), Or(A, B)))
    val f: Formula = Eqv(Or(And(A, B), C), Cond(A, Cond(B, C)))
    println(s"Values that satisfy $f are")
    val res = f.generateTruthTable._3
    val com = f.generateTruthTable._2
    val vars = f.generateTruthTable._1
    val result = (com zip res).filter(_._2==T) map {_._1}
    println(vars)
    println("--------")
    result.foreach(println)
}
