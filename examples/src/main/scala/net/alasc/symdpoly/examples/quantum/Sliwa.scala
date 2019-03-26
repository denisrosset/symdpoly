package net.alasc.symdpoly
package examples.quantum

import defaults._
import net.alasc.symdpoly.evaluation.Evaluator
import shapeless.Witness

object SliwaData {

  // coefficients of the 46 inequalities from C. Sliwa, Phys. Lett. A317, 165 (2003).
  val coefficients = Seq(
    Seq(1, -1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Seq(2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, -1, 0, 0, 0, 0, 0, -1, 0, 1, 0),
    Seq(2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1),
    Seq(2, -2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1, 0),
    Seq(3, -1, 0, -1, 0, -1, 0, -1, 1, -1, 0, -1, 0, 1, 1, -1, 1, 0, 0, -1, 1, -1, 1, 0, 1, 0, -1),
    Seq(3, -1, 0, -1, -1, 0, 0, 0, 0, -1, 0, -1, 0, 1, 1, -1, 1, 0, 0, -1, 1, 1, 0, -1, -1, 1, 0),
    Seq(4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -3, -1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1),
    Seq(4, 0, 0, 0, -1, -1, 0, -1, -1, 0, 0, 0, 0, -2, 0, 0, 0, 2, 0, 0, 0, 0, -1, 1, 0, 1, -1),
    Seq(4, 0, 0, 0, -1, -1, 0, -1, -1, 0, 0, 0, 0, -2, 0, 0, 2, 0, 0, 0, 0, 0, -1, 1, 0, -1, 1),
    Seq(4, 0, 0, 0, -1, -1, 0, -1, -1, 0, -1, 1, -1, -1, 0, 1, 0, 1, 0, -1, 1, 1, 0, -1, -1, 1, 0),
    Seq(4, 0, 0, 0, -2, 0, 0, 0, -2, 0, 0, 0, 0, -1, -1, 0, 1, 1, 0, 0, 0, 0, -1, 1, 0, -1, 1),
    Seq(4, 0, 0, 0, -2, 0, 0, 0, -2, 0, -1, -1, 1, 0, -1, 1, 1, 0, 0, -1, -1, 1, 0, 1, 1, -1, 0),
    Seq(4, 0, 0, 0, -2, -2, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1),
    Seq(4, 0, 0, 0, -2, -2, 0, 0, 0, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, -1, 1, 0, 0, 0, 0, 1, -1),
    Seq(4, 0, 0, 0, -2, -2, 0, 0, 0, 0, -1, -1, 2, 0, 0, 0, -1, 1, 0, -1, -1, 2, 0, 0, 0, 1, -1),
    Seq(4, -1, -1, 0, -1, -1, 0, 0, 0, 0, -1, -1, 0, 0, 2, 0, -1, 1, 0, 0, 0, 0, -1, 1, 0, 1, -1),
    Seq(4, -1, -1, 0, -1, -1, 0, 0, 0, 0, -1, -1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -2, 2),
    Seq(4, -1, -1, 0, -1, -1, 0, 0, 0, 0, -1, -1, 2, 0, 0, 0, -1, 1, 0, 0, 0, 0, -1, 1, -2, 1, 1),
    Seq(4, -1, -1, 0, -1, -1, 0, 0, 0, 0, -1, -1, 2, 0, 0, -2, 1, 1, 0, 0, 0, 0, -1, 1, 0, -1, 1),
    Seq(4, -1, -1, 0, -1, 1, 0, -1, 1, 0, -1, 1, 1, -1, -1, 1, -1, -1, 0, 0, 0, -1, 1, 1, 1, -1, -1),
    Seq(4, -1, -1, -1, -1, 0, -1, 0, 1, 0, -1, -1, -1, 2, 1, -1, 1, 0, 0, 0, 0, 0, -1, 1, 0, 1, -1),
    Seq(4, -1, -1, -1, -1, 0, -1, 0, 1, -1, -1, 0, -1, 2, 1, 0, 1, -1, -1, 0, 1, 0, 1, -1, 1, -1, 0),
    Seq(4, -1, -1, -1, 1, 1, -1, 1, 1, 0, -1, 1, 0, 1, -1, 0, 1, -1, 0, 0, 0, -1, 1, 1, 1, -1, -1),
    Seq(5, -1, 0, -1, 0, -1, 0, -1, -1, -1, 0, -1, 1, -2, 1, 0, 0, 2, 0, -1, -1, 0, 0, 2, 0, 1, -1),
    Seq(5, -1, 0, -1, 0, -1, 0, -1, -1, -1, 0, -1, 1, -2, 1, 0, 0, 2, 0, -1, -1, 0, 2, 0, 0, -1, 1),
    Seq(5, -1, 0, -1, -1, 0, 0, 0, -2, -1, -1, 0, -1, 1, 0, 0, 0, 2, 0, 0, -2, 0, 0, 2, 2, -2, 0),
    Seq(5, -2, -1, -1, 1, 0, 0, -1, -1, -1, 1, 0, 0, -2, 2, -1, 1, 0, 0, -1, -1, -1, 1, 0, -1, 2, 1),
    Seq(6, -1, -1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 1, -2, -1, -1, 1, 2, 0, 0, 0, -1, 1, 2, -1, 3, 0),
    Seq(6, -1, -1, 0, -1, 1, 0, 0, 0, 0, -1, 1, 1, -2, -1, -1, 1, 2, 0, 0, 0, -1, 3, 0, -1, 1, 2),
    Seq(6, -1, -1, 0, -2, 2, 0, -1, 1, 0, -1, 1, 1, -2, -1, 1, -1, -2, 0, 0, 0, -1, 2, 1, 1, -2, -1),
    Seq(6, -1, -1, -1, 0, 1, -1, 1, 0, 0, -1, 1, 0, 0, -2, 0, 1, -3, 0, 0, 0, -1, 2, 1, 1, -2, -1),
    Seq(6, -1, -1, -1, 0, 1, -1, 1, 0, 0, -2, 2, 0, 0, -2, 0, 0, -2, 0, -1, 1, 1, -2, -1, -1, 1, 2),
    Seq(6, -1, -1, -1, 0, 1, -1, 1, 0, -1, 0, 1, 0, 0, -2, 1, -2, -1, -1, 1, 0, 1, -2, -1, 0, -1, 3),
    Seq(6, -1, -1, -1, 0, 1, -1, 1, 0, -1, 0, 1, 1, 2, -1, 2, -2, -2, -1, 1, 0, 2, 0, 0, 1, -1, 2),
    Seq(6, -1, -1, -1, 1, 2, -1, 2, 1, 0, -1, 1, 0, 1, -1, 0, 2, -2, 0, 0, 0, -1, 2, 1, 1, -2, -1),
    Seq(6, -2, 0, 0, -1, -1, 0, -1, -1, 0, -1, -1, -1, 2, -1, 1, -1, 2, 0, -1, -1, 1, -1, 2, 1, -2, 1),
    Seq(6, -2, 0, 0, -1, -1, 0, -1, -1, 0, -1, -1, -1, 3, 0, 1, -2, 1, 0, -1, -1, 1, -2, 1, 1, -1, 2),
    Seq(6, -2, 0, 0, -2, -2, 0, 0, 0, 0, -1, -1, 1, -1, 2, -1, 2, -1, 0, -1, -1, 1, -1, 2, 1, -2, 1),
    Seq(6, -2, 0, -2, 1, -1, 0, -1, -1, -2, 1, -1, 1, -2, 1, -1, 1, 2, 0, -1, -1, -1, 1, 2, -1, 2, -1),
    Seq(6, -2, -2, -2, 1, 1, 0, -1, -1, 0, -1, -1, -2, 1, 1, -2, 2, 2, 0, -1, 1, 0, 2, -2, 0, -1, 1),
    Seq(7, -1, 0, -1, -1, 0, 0, 0, 0, -1, 0, -1, 0, 3, 1, -1, 1, 2, 0, -1, 1, -1, 4, -1, 1, -1, -2),
    Seq(8, -1, -1, -1, -1, 0, -1, 0, 1, 0, -1, 1, -1, 2, 1, 1, 1, -4, 0, 0, -2, 0, 1, 3, -2, 3, 1),
    Seq(8, -2, 0, -2, 1, -1, 0, -1, 1, 0, -1, -1, -1, 2, 3, 1, -1, -2, 0, -1, 1, -1, 3, 0, -1, 4, -1),
    Seq(8, -2, -2, 0, -2, 2, 0, 0, 0, 0, -1, 1, 2, -2, -2, -2, 1, 3, 0, -1, 1, 2, -2, -2, 2, -3, -1),
    Seq(8, -3, -1, 0, -2, 2, 0, -1, 1, 0, -2, 2, 2, -2, -2, 2, -2, -2, 0, -1, 1, 2, -2, -2, -2, 3, 1),
    Seq(10, -3, -1, -3, 2, 1, -1, 1, 2, 0, -2, 2, -1, 3, -4, -1, 1, -2, 0, -1, -1, -2, 3, 1, 2, -4, -2)
  )

  // bounds from https://journals.aps.org/pra/abstract/10.1103/PhysRevA.95.022111
  // Almost-quantum correlations and their refinements in a tripartite Bell scenario
  // James Vallins, Ana Belén Sainz, and Yeong-Cherng Liang
  // Phys. Rev. A 95, 022111, 2017
  val bounds = Mat.rowMajor[Double](46, 14)(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 4, 4, 4, 4 , 2.8284, 2.8284, 2.8284, 2.0000, 2.8284, 2.8284, 2.8284, 2.0000,
    3, 2, 2.8284, 2.8284, 2.8284, 4, 2.0000, 2.8284, 2.8284, 2.0000, 2.0000, 2.8284, 2.8284, 2.0000,
    4, 2, 3.6569, 3.6569, 3.6569, 6, 3.6569, 2, 2, 2, 3.6569, 2, 2, 2,
    5, 3, 4.8885, 4.8885, 4.8885, 7, 4.6569, 4.6569, 4.6569, 3.2097, 4.6569, 4.6569, 4.6569, 3.0187,
    6, 3, 4.6569, 4.6569, 4.6617, 7, 4.6569, 4.6569, 3.0000, 3.0000, 4.6569 , 4.6569, 3.0000, 3.0000,
    7, 4, 20.0/3, 6.6667, 6.6667, 10, 5.6569, 5.6569, 5.6569, 4.0000, 5.6569, 5.6569, 5.6569, 4.0000,
    8, 4, 6.6667, 6.6667, 6.6667, 8, 5.6569, 5.6569, 5.6569, 4.0000, 5.6569 , 5.6569, 5.6569, 4.0000,
    9, 4, 5.6569, 5.6569, 5.6569, 8, 5.6569, 4.0000, 5.6569, 4.0000, 5.6569 , 4.0000, 5.6569, 4.0000,
    10, 4, 4, 4, 5.3211, 20.0/3, 4, 4, 4, 4, 4, 4, 4, 4,
    11, 4, 5.6569, 5.6569, 5.6569, 8, 4.0000, 4.0000, 5.6569, 4.0000, 4.0000 , 4.0000, 5.6569, 4.0000,
    12, 4, 5.6569, 5.6569, 5.6569, 8, 4.3695, 4.3695, 5.6569, 4.2830, 4.0085 , 4.0088, 5.6569, 4.0007,
    13, 4, 5.6569, 5.6569, 5.6569, 8, 5.6569, 4.0000, 5.6569, 4.0000, 5.6569 , 4.0000, 5.6569, 4.0000,
    14, 4, 5.6569, 5.6569, 5.6569, 8, 4.0000, 4.0000, 5.6569, 4.0000, 4.0000 , 4.0000, 5.6569, 4.0000,
    15, 4, 6.0000, 6.0000, 6.0000, 8, 5.6569, 4.4517, 5.6569, 4.2243, 5.6569, 4.0095, 5.6569, 4.0000,
    16, 4, 6.1289, 6.1289 , 6.1289, 8, 5.6569, 5.6569, 5.6569, 4.0000, 5.6568 , 5.6569, 5.6569, 4.0000,
    17, 4, 5.6569, 5.6569, 5.6569, 8, 4.0000, 5.6569, 5.6569, 4.0000, 4.0000, 5.6569, 5.6569, 4.0000,
    18, 4, 5.7538, 5.7538, 5.7538, 8, 5.6569, 4.3130, 4.3130, 4.2247, 5.6569 , 4.0000 , 4.0000, 4.0000,
    19, 4, 5.7829, 5.7829, 5.7829, 8, 5.6569, 5.6569, 4.3063, 4.1865, 5.6569 , 5.6569 , 4.0000, 4.0000,
    20, 4, 6.4853, 6.4853, 6.4853 , 10, 6.4853, 4.5000, 4.6903, 4.1328 , 6.4853, 4.5000, 4.6847, 4.0000,
    21, 4, 5.9555, 5.9555, 5.9555, 60.0/7, 5.6569, 5.6569, 5.6569, 4.1749, 5.6569, 5.6569, 5.6569, 4.0000,
    22, 4, 6.1980, 6.1980, 6.1980, 8, 5.6569, 5.6569, 5.6569, 4.2748, 5.6569 , 5.6569, 5.6569, 4.0000,
    23, 4, 4.6847, 4.7754, 5.2939, 8, 4.5000, 4.5000, 4.6847, 4.1135, 4.5000 , 4.5000, 4.6847, 4.0000,
    24, 5, 7.9401, 7.9401, 7.9401, 31.0/3, 6.6569, 6.6569, 6.6569, 5.2372, 6.6569 , 6.6569, 6.6569, 5.0000,
    25, 5, 6.8243, 6.8243, 6.8415, 31.0/3 , 6.6569, 6.6569, 6.4272, 5.1652, 6.6569 , 6.6569, 6.4272, 5.0000,
    26, 5, 7.9282, 7.9282, 7.9282, 31.0/3, 6.4272, 6.4272, 6.4272, 5.1819, 6.4272 , 6.4272, 6.4272, 5.0000,
    27, 5, 6.9547, 6.9547, 6.9588, 31.0/3, 6.4272, 6.6569, 6.6569, 5.1808, 6.4272, 6.6569, 6.6569, 5.0000,
    28, 6, 9.9098, 9.9098, 9.9098, 14, 9.3137, 7.4272, 7.4272, 6.2123, 9.3137 , 7.4272, 7.4272, 6.0000,
    29, 6, 9.3137, 9.3137, 9.3137, 14, 9.3137, 7.4272, 7.4272, 6.1624, 9.3137 , 7.4272, 7.4272, 6.0000,
    30, 6, 9.3137, 9.3137, 9.3137, 14, 9.3137, 7.4272, 7.4272, 6.1723, 9.3137 , 7.4272, 7.4272, 6.0000,
    31, 6, 7.8043, 7.8043, 7.9226, 12, 7.6569, 7.4272, 7.4272, 6.1866, 7.6569, 7.4272, 7.4272, 6.0000,
    32, 6, 8.1516, 8.1516, 8.1754, 12, 7.6569, 7.6569, 7.4272, 6.2086, 7.6569, 7.6569, 7.4272, 6.0000,
    33, 6, 9.7899, 9.7899, 9.7899, 12, 7.6569, 7.6569, 7.6569, 6.3217, 7.6569, 7.6569, 7.6569, 6.0000,
    34, 6, 8.2515, 8.2515, 8.2723, 12, 7.6569, 7.4272, 7.4272, 6.2444, 7.6569 , 7.4272, 7.4272, 6.0000,
    35, 6, 7.8553, 7.8553, 8.0776 , 12, 7.4272, 7.4272, 7.4272, 6.1794, 7.4272, 7.4272, 7.4272, 6.0000,
    36, 6, 9.4614, 9.4614, 9.4614, 14, 9.3137, 7.4272, 7.4272, 6.1904, 9.3137, 7.4272, 7.4272, 6.0000,
    37, 6, 9.3137, 9.3137, 9.3137, 14, 9.3137, 7.4272, 7.4272, 6.1817, 9.3137 , 7.4272, 7.4272, 6.0000,
    38, 6, 9.3137, 9.3137, 9.3137, 14, 9.3137, 7.4272, 7.4272, 6.1627, 9.3137 , 7.4272, 7.4272, 6.0000,
    39, 6, 9.3253, 9.3253, 9.3253, 12, 7.6569, 7.6569, 7.6569, 6.4378, 7.6569, 7.6569, 7.6569, 6.0000,
    40, 6, 8.1298, 8.1298, 8.1458, 12, 7.4272, 7.6569, 7.4272, 6.2677, 7.4272 , 7.6569, 7.4272, 6.0000,
    41, 7, 10.3677, 10.3735, 10.3769, 15, 10.3137, 10.3137, 8.4272, 7.2012, 10.3137, 10.3137, 8.4272, 7.0000,
    42, 8, 13.0470, 13.0470, 13.0470 , 16, 10.9852, 10.9852, 11.3137, 8.2933, 10.9852, 10.9852, 11.3137, 8.0000,
    43, 8, 11.3137, 11.3137, 11.3137, 16, 10.9852, 9.4272, 11.3137, 8.2481, 10.9852 , 9.4272, 11.3137, 8.0000,
    44, 8, 12.9706, 12.9706, 12.9706, 20, 12.9706, 9.3693, 9.3693, 8.2812, 12.9706, 9.3693, 9.3693, 8.0000,
    45, 8, 12.9706, 12.9706, 12.9706, 20, 12.9706, 9.3693, 9.3693, 8.2675, 12.9706 , 9.3693, 9.3693, 8.0000,
    46, 10, 12.9852, 12.9852, 13.2668, 62.0/3, 12.8543, 12.8543, 12.9852, 10.4006, 12.8543, 12.8543, 12.9852 , 10.0000
  )

}
/** Facet inequalities in the scenario with three parties and binary inputs/outputs.
  *
  * Described in
  *
  * C. Śliwa, Physics Letters A 317, 165 (2003), see also https://arxiv.org/abs/quant-ph/0305190
  *
  */
object Sliwa {

  object Free extends free.MonoidDef(2) {

    case class A(x: Int) extends HermitianOp

    object A extends HermitianOpFamily1(0 to 1)

    case class B(y: Int) extends HermitianOp

    object B extends HermitianOpFamily1(0 to 1)

    case class C(z: Int) extends HermitianOp

    object C extends HermitianOpFamily1(0 to 1)

    val operators = Seq(A, B, C)
  }

  import Free.{A, B, C}

  val Quotient = Free.quotientMonoid(quotient.pairs {
    // parties commute
    case (B(y), A(x)) => A(x) * B(y)
    case (C(z), A(x)) => A(x) * C(z)
    case (C(z), B(y)) => B(y) * C(z)
    // operators are projective measurements with +/- 1 eigenvalues, thus square to identity
    case (A(x1), A(x2)) if x1 == x2 => Free.one
    case (B(y1), B(y2)) if y1 == y2 => Free.one
    case (C(z1), C(z2)) if z1 == z2 => Free.one
    case (op1, op2) => op1 * op2
  })

  // We now describe the feasibility group, i.e. the group that respects the structure of the quotient monoid

  /** Transposition of Alice and Bob. */
  val pT = Free.permutation {
    case A(x) => B(x)
    case B(y) => A(y)
    case op => op
  }

  /** Cyclic permutation Alice -> Bob -> Charlie -> Alice. */
  val pC = Free.permutation {
    case A(x) => B(x)
    case B(y) => C(y)
    case C(z) => A(z)
  }

  /** Flip of Alice's input. */
  val iA = Free.permutation {
    case A(x) => A(1 - x)
    case op => op
  }

  /** Flip Alice output for x = 0. */
  val oA0 = Free.permutation {
    case A(x) if x == 0 => -A(x)
    case op => op
  }

  /** Default evaluator. */
  val L = Quotient.evaluator(evaluation.real)
  /** Evaluator for states with positive partial transpose. */
  val LptA = Quotient.evaluator(evaluation.partialTransposes[Quotient.type, Free.type](Free.A, Free.B ++ Free.C)(Quotient.witness))
  val LptB = Quotient.evaluator(evaluation.partialTransposes[Quotient.type, Free.type](Free.B, Free.A ++ Free.C)(Quotient.witness))
  val LptC = Quotient.evaluator(evaluation.partialTransposes[Quotient.type, Free.type](Free.C, Free.A ++ Free.B)(Quotient.witness))
  val LptAll = Quotient.evaluator(evaluation.partialTransposes[Quotient.type, Free.type](Free.A, Free.B, Free.C)(Quotient.witness))

  /** Group that preserves the problem structure. */
  val feasibilityGroup = Quotient.groupInQuotient(Grp(iA, oA0, pT, pC))

  def npaLevel(l: Int): GSet[Quotient.type] = Quotient.quotient(GSet.onePlus(A, B, C).pow(l))

  def localLevel(l: Int): GSet[Quotient.type] = Quotient.quotient(GSet.onePlus(A).pow(l) * GSet.onePlus(B).pow(l) * GSet.onePlus(C).pow(l))


  val listOfMonomials: Seq[Quotient.Monomial] = for {
    c <- Seq[Free.Monomial](Free.one, C(0), C(1))
    b <- Seq[Free.Monomial](Free.one, B(0), B(1))
    a <- Seq[Free.Monomial](Free.one, A(0), A(1))
  } yield Quotient.quotient(a * b * c)

}

class SliwaInequality(val index0: Int) {
  import SliwaData.{coefficients, bounds}
  import Sliwa.{listOfMonomials, Quotient}
  def index1: Int = index0 + 1
  def expression: Sliwa.Quotient.Polynomial =
    (coefficients(index0).tail zip listOfMonomials.tail).foldLeft(Quotient.zero.toPoly) {
      case (acc, (coeff, mono)) => acc + mono * coeff
    }

  def local: Double = bounds(index0, 1)
  def quantum: Double = bounds(index0, 2)
  def almostQuantum: Double = bounds(index0, 3)
  def npaLevel2: Double = bounds(index0, 4)
  def nonSignaling: Double = bounds(index0, 5)
  def almostQuantumTA: Double = bounds(index0, 6)
  def almostQuantumTB: Double = bounds(index0, 7)
  def almostQuantumTC: Double = bounds(index0, 8)
  def almostQuantumTall: Double = bounds(index0, 9)
  def localLevel6TA: Double = bounds(index0, 10)
  def localLevel6TB: Double = bounds(index0, 11)
  def localLevel6TC: Double = bounds(index0, 12)
  def localLevel6Tall: Double = bounds(index0, 13)
}

object SliwaInequality {
  def fromIndex0(index0: Int): SliwaInequality = new SliwaInequality(index0)
  def fromIndex1(index1: Int): SliwaInequality = fromIndex0(index1 - 1)
}

object SliwaPPT extends App {
  import Sliwa._

  val generatingSet = localLevel(1)
  //  println(localLevel(6).toSortedSet.size)
  val index0 = 43
  val sliwa = SliwaInequality.fromIndex0(index0)
  val expression = sliwa.expression
  //println(sliwa.expression)
  val obj = -expression
  val symmetryGroup = obj.invariantSubgroupOf(feasibilityGroup)
  val generator = symmetryGroup.generators.head
  //println(symmetryGroup.order)
  println(generator)
  val LptAsym = Quotient.symmetricEvaluator(symmetryGroup, evaluation.partialTransposes[Quotient.type, Free.type](Free.A, Free.B ++ Free.C)(Quotient.witness))
  import spire.compat._
  val elements = for {
    a <- Quotient.quotient(GSet.onePlus(Free.A).pow(2)).toSortedSet.toSeq
    bc <- Quotient.quotient(GSet.onePlus(Free.B, Free.C).pow(2)).toSortedSet.toSeq
    set = Set(a * bc, a.adjoint * bc, a * bc.adjoint, a.adjoint * bc.adjoint).flatMap(x => Set(x, x <|+| generator))
    can = set.map(_.phaseCanonical)
    res = if (can.size != set.size) Quotient.zero else set.minBy(_.phaseCanonical)
  } yield (LptAsym(a * bc).normalForm == res)
  elements.foreach(println)

}
