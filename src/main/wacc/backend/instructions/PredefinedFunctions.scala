
object PredefinedFunctions {
    def predefinedFunctions(func: String): StringBuilder = func match {
        case "_println" => ???
        case "_prints" => ???
        case "_printc" => ???
        case "_printi" => ???
        case "printb" => ???
    }  

    def _println() = {
        """
        _println:
            // push {lr}
            stp lr, xzr, [sp, #-16]!
            adr x0, .L._println_str0
            bl puts
            mov x0, #0
            bl fflush
            // pop {lr}
            ldp lr, xzr, [sp], #16
            ret
        """
    }

    def _printi() = {
        """
        _printi:
            // push {lr}
            stp lr, xzr, [sp, #-16]!
            mov x1, x0
            adr x0, .L._printi_str0
            bl printf
            mov x0, #0
            bl fflush
            // pop {lr}
            ldp lr, xzr, [sp], #16
            ret
        """
    }

    def _printc() = {
        """
        _printc:
            // push {lr}
            stp lr, xzr, [sp, #-16]!
            mov x1, x0
            adr x0, .L._printc_str0
            bl printf
            mov x0, #0
            bl fflush
            // pop {lr}
            ldp lr, xzr, [sp], #16
            ret
        """
    }
    
    def _prints() = {
        """
        _prints:
            // push {lr}
            stp lr, xzr, [sp, #-16]!
            mov x2, x0
            ldur w1, [x0, #-4]
            adr x0, .L._prints_str0
            bl printf
            mov x0, #0
            bl fflush
            // pop {lr}
            ldp lr, xzr, [sp], #16
            ret
        """
    }

    def _printb() = {
        """
        _printb:
            // push {lr}
            stp lr, xzr, [sp, #-16]!
            cmp w0, #0
            b.ne .L_printb0
            adr x2, .L._printb_str0
            b .L_printb1
        .L_printb0:
            adr x2, .L._printb_str1
        .L_printb1:
            ldur w1, [x2, #-4]
            adr x0, .L._printb_str2
            bl printf
            mov x0, #0
            bl fflush
            // pop {lr}
            ldp lr, xzr, [sp], #16
            ret
        """
    }

}



