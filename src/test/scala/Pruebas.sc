import Matrices._
import Benchmark._

/******************************************************************************
 * FUNCIÓN:                multMatrizRec y multMatrizRecPar
 ******************************************************************************/
for(i <- 1 to 10; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multMatrizRec, multMatrizRecPar)(m1, m2))