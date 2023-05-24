import Benchmark._
import Matrices._

/******************************************************************************
 * FUNCIÓN:                multMatriz y multMatrizPar
 ******************************************************************************/
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multMatriz, multMatrizPar)(m1, m2))
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multMatriz, multMatrizPar)(m1, m2))
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multMatriz, multMatrizPar)(m1, m2))
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multMatriz, multMatrizPar)(m1, m2))
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multMatriz, multMatrizPar)(m1, m2))

/******************************************************************************
 * FUNCIÓN:                multMatrizRec y multMatrizRecPar
 ******************************************************************************/
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multMatrizRec, multMatrizRecPar)(m1, m2))
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multMatrizRec, multMatrizRecPar)(m1, m2))
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multMatrizRec, multMatrizRecPar)(m1, m2))
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multMatrizRec, multMatrizRecPar)(m1, m2))
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multMatrizRec, multMatrizRecPar)(m1, m2))

/******************************************************************************
 * FUNCIÓN:                multStrassen y multStrassenPar
 ******************************************************************************/
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multStrassen, multStrassenPar)(m1, m2))
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multStrassen, multStrassenPar)(m1, m2))
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multStrassen, multStrassenPar)(m1, m2))
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multStrassen, multStrassenPar)(m1, m2))
for(i <- 1 to 10 ; m1 = matrizAlAzar(math.pow(2, i).toInt, 2); m2 = matrizAlAzar(math.pow(2, i).toInt, 2)) yield (compararAlgoritmos(multStrassen, multStrassenPar)(m1, m2))


