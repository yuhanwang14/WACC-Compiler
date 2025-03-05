package instructions

object AsmLabeling {
  object asmLocal {
    def ~(label: String | Int | PredefinedFunc) = f".L${label.toString}"
  }

  object asmGlobal {
    def ~(label: String | Int | PredefinedFunc) = f"${label.toString}"
  }
}

