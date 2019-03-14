package net.alasc.symdpoly
package mosek

import scala.annotation.tailrec

import spire.syntax.cfor._

import scalin.Sparse
import scalin.immutable.{Mat, Vec}
import scalin.immutable.dense._
import scalin.syntax.all._

import net.alasc.symdpoly.solvers._

class MosekInstance(val format: MosekFormat) {

  def populateTask(task: _root_.mosek.Task): Unit = {
    import format._

    /** Constraints */
    task.appendcons(con.totalDimension)
    assert(con.elements.forall(_.cone == "L="))

    /** Declare PSD variables */
    task.appendbarvars(psdVar.elements.map(_.n).toArray)

    /** Declare and constraint scalar variables in their respective cones. */
    {
      task.appendvars(`var`.n)
      `var`.elements.foldLeft(0) {
        case (shift, VarElement("F", n)) => shift + n
        case (shift, VarElement("L+", n)) =>
          cforRange(shift until shift + n) { j =>
            val lower = 1
            val finite = 1
            task.chgvarbound(j, lower, finite, 0.0)
          }
          shift + n
        case (shift, VarElement(cone, n)) => sys.error(s"Unsupported cone type $cone")
      }
    }

    /** Set objective */
    {
      task.putcfix(objBCoord.real)
      // scalar part
      for ( ObjACoordElement(j, cj) <- objACoord.elements )
        task.putcj(j, cj)
      // PSD part
      val num = objFCoord.elements.size
      val subj = objFCoord.elements.map(_.j).toArray
      val subk = objFCoord.elements.map(_.r).toArray
      val subl = objFCoord.elements.map(_.c).toArray
      val valjkl = objFCoord.elements.map(_.real).toArray
      task.putbarcblocktriplet(num, subj, subk, subl, valjkl)
    }

    /** Set constraints */
    {
      cforRange(0 until con.totalDimension) { i =>
        task.putconbound(i, _root_.mosek.boundkey.fx, 0.0, 0.0)
      }
      for ( BCoordElement(i, real) <- format.bCoord.elements ) {
        val finite = 1
        val lower = 1
        val upper = 0
        task.chgconbound(i, lower, finite, -real)
        task.chgconbound(i, upper, finite, -real)
      }
      for ( ACoordElement(i, j, real) <- format.aCoord.elements )
        task.putaij(i, j, real)
      val num = fCoord.elements.size
      val subi = fCoord.elements.map(_.i).toArray
      val subj = fCoord.elements.map(_.j).toArray
      val subk = fCoord.elements.map(_.r).toArray
      val subl = fCoord.elements.map(_.c).toArray
      val valijkl = fCoord.elements.map(_.real).toArray
      task.putbarablocktriplet(num, subi, subj, subk, subl, valijkl)
    }

  }

  def writeFile(fileName: String, tolRelGap: Double = 1e-9): Unit = {
    import resource._
    for {
      env <- managed(new _root_.mosek.Env)
      task <- managed(new _root_.mosek.Task(env))
    } {
      task.putdouparam(_root_.mosek.dparam.intpnt_co_tol_rel_gap, tolRelGap)
      task.set_Stream(_root_.mosek.streamtype.log, new _root_.mosek.Stream {
        def stream(msg: String): Unit = System.out.print(msg)
      })
      populateTask(task)
      task.writedata(fileName)
    }
  }


  def solve(tolRelGap: Double = 1e-9): Solution = {
    import resource._
    var res: Solution = null
    for {
      env <- managed(new _root_.mosek.Env)
      task <- managed(new _root_.mosek.Task(env))
    } {
      task.putdouparam(_root_.mosek.dparam.intpnt_co_tol_rel_gap, tolRelGap)
      task.set_Stream(_root_.mosek.streamtype.log, new _root_.mosek.Stream {
        def stream(msg: String): Unit = System.out.print(msg)
      })
      populateTask(task)
      task.optimize
      /* Print a summary containing information
         about the solution for debugging purposes*/
      task.solutionsummary(_root_.mosek.streamtype.msg)
      val solsta = new Array[_root_.mosek.solsta](1)
      task.getsolsta(_root_.mosek.soltype.itr, solsta)
      val Optimal = _root_.mosek.solsta.optimal
      val NearOptimal = _root_.mosek.solsta.near_optimal
      val DualInfeasCer = _root_.mosek.solsta.dual_infeas_cer
      val PrimInfeasCer = _root_.mosek.solsta.prim_infeas_cer
      val NearDualInfeasCer = _root_.mosek.solsta.near_dual_infeas_cer
      val NearPrimInfeasCer = _root_.mosek.solsta.near_prim_infeas_cer
      val Unknown = _root_.mosek.solsta.unknown

      res = solsta(0) match {
        case Optimal | NearOptimal =>
          val primalobj = new Array[Double](1)
          val dualobj = new Array[Double](1)
          task.getprimalobj(_root_.mosek.soltype.itr, primalobj)
          task.getdualobj(_root_.mosek.soltype.itr, dualobj)
          OptimumFound(Some(primalobj(0)), dualobj(0))
            /*
          val barx = new Array[Double](lenbarvar(0))
          task.getbarxj(_root_.mosek.soltype.itr, /* Request the interior solution. */ 0, barx)
          val y = new Array[Double](n)
          task.gety(_root_.mosek.soltype.itr, y)
          val X = MosekInstance1.fromLowerTriangularColStacked(d, Vec.fromSeq(barx))
          val yvec = Vec.tabulate(n + 1) {
            case 0 => 1
            case i => y(i - 1)
          }

          @tailrec def iter(i: Int, acc: Double): Double =
            if (i == n) acc else iter(i + 1, acc + y(i) * blc(i))
*/
        case DualInfeasCer | PrimInfeasCer | NearDualInfeasCer | NearPrimInfeasCer =>
          Failure("Primal or dual infeasibility certificate found.")
        case Unknown =>
          Failure("The status of the solution could not be determined.")
        case _ =>
          Failure("Other solution status.")
      }
    }
    res
  }
}
