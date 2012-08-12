package GrowCode
package MachineOperations

/**
 * Companion class for abstract MachineOperation class
 */
object MachineOperation{

  val random = scala.util.Random

  /**
   * Given a list of tokens (presumably parsed from a line in a program file),
   * return an appropriate concrete instance of a MachineOperation class
   */
  def fromTokenList( s: List[String] ) : MachineOperation = {
    val op =
      s match {
        case "noop" :: Nil => Some( NoOp() )
        case "copy" :: Nil => Some( CopyOp() )
        case "detach" :: Nil => Some( DetachOp() )
        case "attach" :: Nil => Some( AttachOp() )
        case "clearinf" :: Nil => Some( RonPaulOp() )
        //case "ronpaul" :: Nil => Some( RonPaulOp() )
        case "save" :: Nil => Some( SaveOp() )
        case "load" :: Nil => Some( LoadOp() )
        case "swap" :: Nil => Some( SwapOp() )
        case "create" :: Nil => Some( CreateOp() )
        case "redge" :: Nil => Some( REdgeOp() )
        case "rnode" :: Nil => Some( RNodeOp() )
        case "neigh" :: Nil => Some( NeighOp() )
        case "truth" :: xs => {
          Some( TruthOp(xs(0).toFloat, xs(1).toFloat, xs(2).toFloat, xs(3).toFloat) )
        }
        case "step" :: xs => Some( StepOp(xs(0).toInt) )
        case "rewind" :: xs => {
	  val params = xs(0).split(",")
	  Some(RewindOp(params(0).toInt, params(1).toInt))
	}
        case "loop" :: Nil => Some( LoopOp() )
        case "walk" :: xs => Some( WalkOp(xs(0).toInt) )
        case "set" :: xs => Some( SetOp(xs(0).toInt) )
        case "new" :: Nil => Some( NewOp() )
        case "infl" :: xs => Some( InflOp(xs(0).toFloat) )
        case "scoin" :: xs => Some( ScoinOp(xs(0).toFloat) )
        case "clear" :: Nil => Some( ClearOp() )
        case _ => Option.empty[MachineOperation]
      }

    if ( op.isEmpty ){
      throw new Exception("Failed to parse cmd: %s".format( s.mkString(" ") ))
    }
    op.get
  }

  val opArray = Array(
//    NoOp(),
//    CopyOp(),
    DetachOp(),
    RonPaulOp(),
    AttachOp(),
    SaveOp(),
    LoadOp(),
    SwapOp(),
    CreateOp(),
    REdgeOp(),
    RNodeOp(),
//    NeighOp(),
//    TruthOp(0.0f, 0.0f, 0.0f, 0.0f),
    //StepOp(0),
    RewindOp(0,0),
//    WalkOp(0),
    SetOp(0),
    //LoopOp(),
    InflOp(0.0f),
    NewOp(),
    ClearOp(),
    ScoinOp(0.0f)
  )

  def randomOperation() = {
    val op = opArray( scala.util.Random.nextInt( opArray.size ) )
    op match {
      case TruthOp(p0,p1,p2,p3) => TruthOp(random.nextFloat, random.nextFloat, random.nextFloat, random.nextFloat )
      case StepOp(s) => StepOp(random.nextInt(10)+1)
      case RewindOp(s,t) => RewindOp(random.nextInt(10)+1, random.nextInt(10)+1)
      case WalkOp(s) => WalkOp(random.nextInt(10)+1)
      case SetOp(s) => SetOp(random.nextInt(10)+1)
      case InflOp(s) => InflOp(random.nextFloat)
      case ScoinOp(s) => ScoinOp(random.nextFloat)
      case x => x
    }
  }

}

abstract class MachineOperation {
  val shortStr = ""
  val Str = ""
  def apply( m: Machine ) : Unit
  def reset : Unit = { }
}

case class NoOp() extends MachineOperation {
  override val shortStr = "."
  override val Str = "noop"
  override def apply( m: Machine ) { m.opNop() }
}

case class CopyOp() extends MachineOperation {
  override val shortStr = ":"
  override val Str = "copy"
  override def apply( m: Machine ) { m.opCopy() }
}

case class DetachOp() extends MachineOperation {
  override val shortStr = "D"
  override val Str = "detach"
  override def apply( m: Machine ) { m.opDetach() }
}

case class AttachOp() extends MachineOperation {
  override val shortStr = "A"
  override val Str = "attach"
  override def apply( m: Machine ) { m.opAttach() }
}

case class RonPaulOp() extends MachineOperation {
  //override val shortStr = "R"
  //override val Str = "ronpaul"
  override val shortStr = "C"
  override val Str = "clearinf"
  override def apply( m: Machine ) { m.opRonPaul() }
}

case class SaveOp() extends MachineOperation{
  override val shortStr = "S"
  override val Str = "save"
  override def apply( m: Machine ) { m.opSave() }
}

case class LoadOp() extends MachineOperation {
  override val shortStr = "L"
  override val Str = "load"
  override def apply( m: Machine ) { m.opLoad() }

}

case class SwapOp() extends MachineOperation {
  override val shortStr = "W"
  override val Str = "swap"
  override def apply( m: Machine ) { m.opSwap() }
}

case class CreateOp() extends MachineOperation {
  override val shortStr = "/"
  override val Str = "create"
  override def apply( m: Machine ) { m.opCreate() }
}

case class REdgeOp() extends MachineOperation {
  override val shortStr = "E"
  override val Str = "redge"
  override def apply( m: Machine ) { m.opRedge() }
}

case class RNodeOp() extends MachineOperation {
  override val shortStr = "V"
  override val Str = "rnode"
  override def apply( m: Machine ) { m.opRnode() }
}

case class NeighOp() extends MachineOperation {
  override val shortStr = "N"
  override val Str = "neigh"
  override def apply( m: Machine ) { m.opNeigh() }
}

case class TruthOp(
  p1: Float,
  p2: Float,
  p3: Float,
  p4: Float) extends MachineOperation {

  override val shortStr = "T"
  override val Str = "truth "+p1+" "+p2+" "+p3+" "+p4
  override def apply( m: Machine ) { m.opTruth(p1,p2,p3,p4) }
}

case class StepOp( var d: Int ) extends MachineOperation {
  val _d = d
  override val shortStr = "[%d".format(d)
  override val Str = "step "+d
  override def apply( m: Machine ) { m.opStep( this ) }
  override def reset { d = _d }
}
  
case class RewindOp( var nInstr: Int, var nLoop: Int ) extends MachineOperation {
  override val shortStr = "<-(%d,%d)".format(nInstr, nLoop)
  override val Str = "rewind "+nInstr+","+nLoop
  override def apply( m: Machine ) { m.opRewind( nInstr, nLoop ) }
}

case class LoopOp() extends MachineOperation {
  override val shortStr = "]"
  override val Str = "loop"
  override def apply( m: Machine ) { m.opNop() } // Not yet implemented
}

case class WalkOp( d: Int ) extends MachineOperation {
  override val shortStr = "~"
  override val Str = "walk "+d
  override def apply( m: Machine ) { m.opWalk(d) }
}

case class SetOp( d: Int ) extends MachineOperation {
  override val shortStr = ">(%d)".format(d)
  override val Str = "set "+d
  override def apply( m: Machine ) { m.opSet(d) }
}
case class NewOp() extends MachineOperation {
  override val shortStr = "*"
  override val Str = "new"
  override def apply( m: Machine ) { m.opNew() }
}

case class InflOp(val p:Float) extends MachineOperation {
  override val shortStr = "!(%f)".format(p)
  override val Str = "infl "+p
  override def apply( m: Machine ) { m.opInfl(p) }
}

case class ScoinOp(val p:Float) extends MachineOperation {
  override val shortStr = "@(%f)".format(p)
  override val Str = "scoin "+p
  override def apply( m: Machine ) { m.opScoin(p) }
}

case class ClearOp() extends MachineOperation {
  override val shortStr = "-"
  override val Str = "clear"
  override def apply( m: Machine ) { m.opClear() }
}
