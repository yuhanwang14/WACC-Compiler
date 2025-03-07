package testcode


object TestConfig {
  // Set this flag to true when running locally, false when on the lab machines.
  val localMode: Boolean = true
  // Set to true when testing valid files
  val testValid: Boolean = true
  // Set to true when testing invalid files
  val testInvalid: Boolean = false
  
  // Define the assembler command based on environment
  val assemblerCmd: String = if (localMode) "aarch64-elf-gcc" else "aarch64-linux-gnu-gcc"
  
  // Define the emulator command and library path based on environment
  val emulatorCmd: String = if (localMode) "qemu-system-aarch64" else "qemu-aarch64"
  val emulatorLibPath: String = if (localMode) "/usr/local/Cellar/qemu/9.2.2/share/qemu" else "/usr/aarch64-linux-gnu/"
}
