package instructions

enum Cond(val mnemonic: String):
  case EQ extends Cond("eq") // Equal
  case NE extends Cond("ne") // Not Equal
  case CS extends Cond("cs") // Carry Set (unsigned higher or same)
  case CC extends Cond("cc") // Carry Clear (unsigned lower)
  case MI extends Cond("mi") // Minus/Negative
  case PL extends Cond("pl") // Plus/Positive or Zero
  case VS extends Cond("vs") // Overflow Set
  case VC extends Cond("vc") // Overflow Clear
  case HI extends Cond("hi") // Unsigned Higher
  case LS extends Cond("ls") // Unsigned Lower or Same
  case GE extends Cond("ge") // Signed Greater Than or Equal
  case LT extends Cond("lt") // Signed Less Than
  case GT extends Cond("gt") // Signed Greater Than
  case LE extends Cond("le") // Signed Less Than or Equal
  case AL extends Cond("al") // Always (unconditional)
  case NV extends Cond("nv") // Never

  override def toString = mnemonic
