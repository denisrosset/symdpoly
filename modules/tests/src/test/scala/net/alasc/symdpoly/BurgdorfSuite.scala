package net.alasc.symdpoly

class BurgdorfSuite extends CommonSuite {

  test("Example 5.13 from Burgdorf, Klep and Povh") {
    import examples.tracial.BurgdorfExample5_13._
    Free
    val OptimumFound(Some(primalObj), dualObj) = program.sdpa.solve()
    val expected = 0.2842
    val tol = 1e-4
    assert((dualObj - expected).abs < tol)
  }

}
