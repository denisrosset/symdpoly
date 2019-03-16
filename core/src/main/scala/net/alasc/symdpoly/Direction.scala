package net.alasc.symdpoly

/** Direction along which to optimize the objective: minimization or maximization. */
sealed trait Direction {
  def reverse: Direction
}

object Direction {
  case object Minimize extends Direction {
    def reverse: Direction = Maximize
  }
  case object Maximize extends Direction {
    def reverse: Direction = Minimize
  }
}
