package testcode

object TestConfig {
  // Set this flag to true when running locally (on macOS), false when on the lab machines.
  val localMode: Boolean = true
  // Set to true when testing valid files
  val testValid: Boolean = true
  // Set to true when testing invalid files
  val testInvalid: Boolean = false

  // Define the assembler command based on environment.
  val assemblerCmd: String = "aarch64-linux-gnu-gcc"
  
  // Define the emulator command.
  val emulatorCmd: String  = "qemu-aarch64"
  
  // Define the emulator library path based on environment.
  val emulatorLibPath: String = "/usr/aarch64-linux-gnu/"
}
