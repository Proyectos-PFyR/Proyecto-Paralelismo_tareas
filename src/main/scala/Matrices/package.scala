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
}
