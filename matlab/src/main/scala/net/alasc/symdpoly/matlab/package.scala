package net.alasc.symdpoly

package object matlab {

  implicit class MatlabRelaxation(val relaxation: Relaxation[_, _, _]) {
    def scsInstance: SCSInstance = new SCSInstance(relaxation)
    def sdpt3Instance: SDPT3Instance = new SDPT3Instance(relaxation)
    def sedumiInstance: SeDuMiInstance = new SeDuMiInstance(relaxation)
  }
}
