package backend.instructions


object AsmLabeling:
  object asmLocal:
    def ~(label: String | Int | PredefinedFunc) = f".L$label"

  object asmGlobal:
    def ~(label: String | Int | PredefinedFunc) = f"$label"
