package instructions

object AsmLabeling {
  object asmLocal {
    def ~(label: String | Int) = f".L$label"
  }

  object asmGlobal {
    def ~(label: String | Int) = f"$label"
  }
}

