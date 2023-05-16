import common._

import scala.util.Random
package object Matrices {
  /** ****************************************************************************
   * AUTORES
   * Samuel Galindo Cuevas - 202177491
   * Nicolas Herrera Marulanda - 202182551
   * **************************************************************************** */

  /** ****************************************************************************
   * Herramientas
   * **************************************************************************** */
    val random = new Random()
    type Matriz =  Vector[Vector[Int]]

  /** ****************************************************************************
   * Funciones entregadas por el profesor
   * **************************************************************************** */
  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    //Crea una matriz de enteros cuadrada de long x long ,
    // con valores aleatorios entre 0 y vals
    val v = Vector.fill(long, long) {random.nextInt(vals)}
    v
  }

  def prodEscalar(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({case (i,j) => (i*j)}).sum
  }

  def transpuesta (m: Matriz): Matriz = {
    val 1=m.length
    Vector.tabulate(1,1)((i,j) => m(j)(i))
  }

  /** ****************************************************************************
   * FUNCIÓN:                subMatriz
   * DESCRIPCIÓN:            Una función que retorna una submatriz de tamano l desde la posicion i, j
   * PARÁMETROS DE ENTRADA:
   * m:                      Matriz generadora
   * i:                      Posicion inicial i (Fila)
   * j:                      Posicion j (Columna)
   * l:                      Tamano submatriz
   * RETORNO
   * Matriz:  			         Submatriz
   * **************************************************************************** */
  def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz =
  {
    Vector.tabulate(l, l)((a, b) => m(a + i)(b + j))
  }

  /** ****************************************************************************
   * FUNCIÓN:                sumMatriz
   * DESCRIPCIÓN:            Una función que retorna una submatriz de tamano l desde la posicion i, j
   * PARÁMETROS DE ENTRADA:
   * m:                      Matriz generadora
   * i:                      Posicion inicial i (Fila)
   * j:                      Posicion j (Columna)
   * l:                      Tamano submatriz
   * RETORNO
   * Matriz:  			         Submatriz
   * **************************************************************************** */
  def sumMatriz(m1: Matriz, m2: Matriz): Matriz =
  {
    Vector.tabulate(m1.length, m1.length)((a, b) => m1(a)(b) + m2(a)(b))
  }

  /** ****************************************************************************
   * FUNCIÓN:                mulMatrizSec
   * DESCRIPCIÓN:            Una función que retorna la matriz resultante de multiplicar m1 y m2
   * PARÁMETROS DE ENTRADA:
   * m1:                     Primera matriz n x n
   * i2:                     Segunda matriz n x n
   * RETORNO
   * Matriz:  			         Matriz resultante de multiplicar m1 y m2
   * **************************************************************************** */
  def mulMatrizSec(m1: Matriz, m2: Matriz): Matriz =
  {
    val n = m1.length

    if (n == 1)
    {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    }
    else
    {
      val m = n / 2

      val a11 = subMatriz(m1, 0, 0, m)
      val a12 = subMatriz(m1, 0, m, m)
      val a21 = subMatriz(m1, m, 0, m)
      val a22 = subMatriz(m1, m, m, m)

      val b11 = subMatriz(m2, 0, 0, m)
      val b12 = subMatriz(m2, 0, m, m)
      val b21 = subMatriz(m2, m, 0, m)
      val b22 = subMatriz(m2, m, m, m)

      val c11 = sumMatriz(mulMatrizSec(a11, b11), mulMatrizSec(a12, b21))
      val c12 = sumMatriz(mulMatrizSec(a11, b12), mulMatrizSec(a12, b22))
      val c21 = sumMatriz(mulMatrizSec(a21, b11), mulMatrizSec(a22, b21))
      val c22 = sumMatriz(mulMatrizSec(a21, b12), mulMatrizSec(a22, b22))

      c11.zip(c12).map { case (filaC11, filaC12) => filaC11 ++ filaC12 } ++ c21.zip(c22).map { case (filaC21, filaC22) => filaC21 ++ filaC22}
    }
  }

  /** ****************************************************************************
   * FUNCIÓN:                mulMatrizRecPar
   * DESCRIPCIÓN:            Una función que retorna la matriz resultante de multiplicar m1 y m2 realizando operaciones en paralelo
   * PARÁMETROS DE ENTRADA:
   * m1:                     Primera matriz n x n
   * i2:                     Segunda matriz n x n
   * RETORNO
   * Matriz:  			         Matriz resultante de multiplicar m1 y m2
   * **************************************************************************** */
  def mulMatrizRecPar1(m1: Matriz, m2: Matriz): Matriz =
  {
    val n = m1.length

    if (n == 1)
    {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    }
    else
    {
      val m = n / 2

      val a11 = subMatriz(m1, 0, 0, m)
      val a12 = subMatriz(m1, 0, m, m)
      val a21 = subMatriz(m1, m, 0, m)
      val a22 = subMatriz(m1, m, m, m)

      val b11 = subMatriz(m2, 0, 0, m)
      val b12 = subMatriz(m2, 0, m, m)
      val b21 = subMatriz(m2, m, 0, m)
      val b22 = subMatriz(m2, m, m, m)

      val c11 = task(sumMatriz(mulMatrizRecPar1(a11, b11), mulMatrizRecPar1(a12, b21)))
      val c12 = task(sumMatriz(mulMatrizRecPar1(a11, b12), mulMatrizRecPar1(a12, b22)))
      val c21 = task(sumMatriz(mulMatrizRecPar1(a21, b11), mulMatrizRecPar1(a22, b21)))
      val c22 = task(sumMatriz(mulMatrizRecPar1(a21, b12), mulMatrizRecPar1(a22, b22)))

      c11.join().zip(c12.join()).map { case (filaC11, filaC12) => filaC11 ++ filaC12 } ++ c21.join().zip(c22.join()).map { case (filaC21, filaC22) => filaC21 ++ filaC22 }
    }
  }



}
