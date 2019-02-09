package net.alasc.symdpoly

package object matlab {

  implicit class MatlabRelaxation(val relaxation: Relaxation[_, _, _]) {
    def scsInstance: SCSInstance = new SCSInstance(relaxation)
    def sdpt3Instance: SDPT3Instance = new SDPT3Instance(relaxation)
    def sedumiInstance: SeDuMiInstance = new SeDuMiInstance(relaxation)
  }

  implicit class MatlabRelaxation2(val relaxation: Relaxation2[_, _]) {
    def scsInstance: SCSInstance2 = new SCSInstance2(relaxation)
    def sdpt3Instance: SDPT3Instance2 = new SDPT3Instance2(relaxation)
    def sedumiInstance: SeDuMiInstance2 = new SeDuMiInstance2(relaxation)
  }

}
