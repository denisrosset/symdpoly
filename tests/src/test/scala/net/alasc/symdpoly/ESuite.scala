package net.alasc.symdpoly

import net.alasc.perms.Perm
import net.alasc.symdpoly.math.{GenPerm, PhasedInt, Phases}
import spire.laws.{ActionLaws, InvolutionLaws, RingLaws}

class PhaseSuite extends CommonSuite {
  checkAll("Phase.multiplicative group", RingLaws[Phase].multiplicativeAbGroup)
  checkAll("Phase.involution", InvolutionLaws[Phase].involutionMultiplicativeMonoid)
}

class PhasesSuite extends CommonSuite {
  import net.alasc.laws.Permutations.arbPerm
  import laws.GenPerms._
  checkAll("IntPhaseSortedMap.group", RingLaws[Phases].abGroup)
  checkAll("IntPhaseSortedMap.involution", InvolutionLaws[Phases].involution)
  checkAll("IntPhaseSortedMap.action", ActionLaws[Phases, PhasedInt].groupAction)
  checkAll("IntPhaseSortedMap perm action", ActionLaws[Perm, Phases].groupAction)
}

class GenPermSuite extends CommonSuite {
  import laws.GenPerms._
  checkAll("group", RingLaws[GenPerm].group)
  checkAll("action", ActionLaws[GenPerm, PhasedInt].groupAction)
}
