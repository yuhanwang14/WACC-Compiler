package instructions

enum Extend(val mnemonic: String):
  case UXTB extends Extend("uxtb")
  case UXTH extends Extend("uxth")
  case LSL extends Extend("lsl")
  case UXTX extends Extend("uxtx")
  case SXTB extends Extend("sxtb")
  case SXTH extends Extend("sxth")
  case SXTW extends Extend("sxtw")
  case SXTX extends Extend("sxtx")

  override def toString = mnemonic