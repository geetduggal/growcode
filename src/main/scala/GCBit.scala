package GrowCode
import MachineOperations._
import ec._
import ec.simple._
import ec.vector._

/**
 * This class represents a custom GrowCode "Bit" for use in ECJ's
 * GeneVectorIndividual.  It extends VectorGene and is a lightweight
 * wrapper around a machine operation.
 */
class GCBit extends VectorGene {

  var op = MachineOperation.randomOperation()

  override def mutate( state: EvolutionState, thread: Int ) {
    reset(state, thread)
  }

  override def reset( state: EvolutionState, thread: Int ) {
    op = MachineOperation.randomOperation()
  }

  override def hashCode() = { op.hashCode() }

  override def equals(that:Any) = that match {
    case that: GCBit => this.op == that.op
    case _ => false
  }

  override def printGeneToStringForHumans() = {
    op.Str+"\n"
  }
}
