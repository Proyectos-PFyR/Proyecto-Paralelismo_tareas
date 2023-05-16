import Matrices._
import Benchmark._

val m1 = Vector(Vector(0, 0), Vector(1, 1))
val m2 = Vector(Vector(1, 0), Vector(1, 1))

val m3 = matrizAlAzar(512,10)
val m4 = matrizAlAzar(512,10)

val m5 = Vector(Vector(2, 1, 0, 2), Vector(0, 0, 1, 0), Vector(0, 2, 1, 1), Vector(0, 2, 2, 2))
val m6 = Vector(Vector(1, 0, 2, 1), Vector(2, 0, 1, 1), Vector(2, 1, 0, 1), Vector(1, 0, 0, 0))

subMatriz(m5,0, 0, m5.length/2)

sumMatriz(m1, m2)

//mulMatrizSec(m3, m4)

mulMatrizSec(m1, m2)

mulMatrizSec(m5, m6)

mulMatrizRecPar1(m5, m6)

compararAlgoritmos(mulMatrizSec, mulMatrizRecPar1)(m3,m4)