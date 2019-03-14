package net.alasc.symdpoly

/*
/** Moment/SOS-based relaxation of a polynomial optimization problem.
  * @param problem       Problem to construct the relaxation for
  * @param generatingSet Set of monomials used to generate the moment matrix
  */
case class OldRelaxation[
  E <: generic.Evaluator.Aux[M] with Singleton:Witness.Aux,
  M <: generic.MonoidDef with Singleton:Witness.Aux,
](problem: Optimization[E, M], generatingSet: GSet[M]) {

  def E: E = valueOf[E]

  val objective: EvaluatedPoly[E, M] = problem.objective

  lazy val momentMatrix: OldMomentMatrix[E, M] = OldMomentMatrix(E, generatingSet)

  lazy val objectiveVector: Vec[Cyclo] = objective.vecOverOrderedSet(momentMatrix.momentSet.elements)

  def isObjectiveReal: Boolean = objectiveVector.toIndexedSeq.forall(c => c.isReal)

  def mosekInstance: OldMosekInstance = new OldMosekInstance(this)

  def sdpaInstance: OldSDPAInstance = new OldSDPAInstance(this)

  /** Writes the Gram matrix indices to the file with given filename.
    * The structure of the file is as follows
    *
    * Line   1: N M
    * Line   2: g11 g12 ... g1N
    * ...
    * Line 1+N: gN1 gN2 ... gNN
    *
    * where N is the size of the Gram matrix, and M is the number of monomials involved,
    * including the identity.
    *
    * The matrix index g(r,c) takes the value
    *
    * -  0 if the element is always 0
    * -  1 if the element takes the constant value  1 or -1
    * -  j if the element corresponds to the expectation value of j-th monomial (with a possible factor -1)
    *
    * and max_{r c} g(r,c) = M
    *
    */
  def momentIndexMatrixDescription: String = {
    import scalin.immutable.dense._
    import momentMatrix._
    val mat = momentIndexMatrix
    s"${matrixSize} ${nUniqueMonomials}\n" ++ Seq.tabulate(matrixSize)( r => mat(r, ::).toIndexedSeq.mkString(" ") ).mkString("\n")
  }

  /** Writes the sign/phase of the monomials present in the Gram matrix. */
  def phaseMatrixDescription: String = {
    import scalin.immutable.dense._
    import momentMatrix._
    val mat = phaseMatrix
    s"${matrixSize}\n" ++ Seq.tabulate(matrixSize)( r => mat(r, ::).toIndexedSeq.mkString(" ") ).mkString("\n")
  }

  def momentMatrixDescription: String = scalin.Printer.mat(momentMatrix.momentMatrix, Int.MaxValue, Int.MaxValue)

}
*/
/*


// TODO: use the symmetry equivalence of the evaluator
def canonicalMonomialsDescription(symmetryGroup: Grp[M#Permutation]): String = {
  val monomials = Vector.tabulate(gramMatrix.momentSet.nElements) { i =>
    import spire.compat._
    val element = gramMatrix.momentSet(i)
    val orbit = symmetryGroup.iterator.map(g => E.evaluatedMonoPermutationAction.actr(element, g)).toSet.toVector.sorted.map(_.normalForm).mkString(", ")
    s"${i} ${element.normalForm}: ${orbit}"
  }
  monomials.mkString("\n")
}

def describeGroup(grp: Grp[GenPerm], printGen: GenPerm => String): String = {
  import net.alasc.perms.default._
  val generators = grp.smallGeneratingSet.map(printGen)
  s"Number of generators: ${generators.length}\n" ++ s"Group order: ${grp.order}\n" ++ generators.mkString("\n")
}

def matrixSymmetryGroupDescription: String = {
  import gramMatrix.matrixSymmetries._
  val genPermGrp = Grp(grp.generators.map(representation): _*)
  describeGroup(genPermGrp, _.toString)
}
*/