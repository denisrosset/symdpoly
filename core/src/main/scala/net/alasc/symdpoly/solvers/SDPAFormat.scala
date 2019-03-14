package net.alasc.symdpoly
package solvers

import java.io.{BufferedWriter, FileWriter, PrintWriter, StringWriter, Writer}

import spire.syntax.cfor.cforRange

case class SDPAFormat(val sdp: SDP) extends TextFormat {
  import sdp._

  val m: Int = obj.length - 1

  def writeData(writer: Writer): Unit =
    if (eqA.nRows > 0) sdp.convertEqualitiesToInequalities.sdpa.writeData(writer) else {
      (direction, obj(0)) match {
        case (Direction.Minimize, 0) =>
        case (Direction.Maximize, 0) =>
          writer.append(s"* SDPA solves a minimization dual problem, while we express a maximization problem.\n")
          writer.append(s"* The real objective is thus -obj_SPDA_value.")
        case (Direction.Maximize, cte) =>
          writer.append(s"* SDPA solves a minimization dual problem, while we express a maximization problem.\n")
          writer.append(s"* Constant terms are not supported by SDPA, but the original objective has constant term cte = ${cte}.\n")
          writer.append(s"* The real objective is thus ${cte} - obj_SDPA_value\n")
        case (Direction.Minimize, cte) =>
          writer.append(s"* Constant terms are not supported by SDPA, but the original objective has constant term cte = ${cte}.\n")
          writer.append(s"* The real objective is thus ${cte} + obj_SDPA_value\n")
      }
      val nBlocks = blocks.length + (if (ineqA.nRows > 0) 1 else 0)
      writer.append(s"$m\n")
      writer.append(s"$nBlocks\n")
      val sdpaBlockSizes = blocks.map(_.size) ++ (if (ineqA.nRows > 0) Seq(-ineqA.nRows) else Seq.empty)
      writer.append(sdpaBlockSizes.mkString("", " ", "\n"))
      direction match {
        case Direction.Maximize =>
          writer.append((1 until obj.length).map(i => -obj(i)).mkString("", " ", "\n"))
        case Direction.Minimize =>
          writer.append((1 until obj.length).map(obj(_)).mkString("", " ", "\n"))
      }
      cforRange(0 until blocks.length) { b =>
        val block = blocks(b)
        cforRange(0 until block.nEntries) { i =>
          val di = block.basisIndex(i)
          val r = block.rowIndex(i)
          val c = block.colIndex(i)
          val e = if (di == 0) -block.coeffs(i) else block.coeffs(i)
          if (c >= r) // upper triangle
            writer.append(s"$di ${b + 1} ${r + 1} ${c + 1} $e\n")
        }
      }
      if (ineqA.nRows > 0) {
        cforRange(0 until ineqA.nRows) { r =>
          cforRange(0 until ineqA.nCols) { c =>
            if (ineqA(r, c) != 0) {
              val e = if (c == 0) -ineqA(r, c) else ineqA(r, c)
              writer.append(s"$c $nBlocks $r $r $e\n")
            }
          }
        }
      }
    }

}
