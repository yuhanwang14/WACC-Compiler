package instructions

object AsmLabeling {
  object asmLocal {
    def ~(label: String) = f".L$label"
  }

  object asmGlobal {
    def ~(label: String) = f"$label"
  }
}

