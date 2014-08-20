basic-assembler
===============

A very basic 68k assembler written in Amiga BASIC, ca 1989-90. This was created by reverse-engineering the 68k machine code - at that time I had access to a disassembler but not an assembler, plus various samples of assembly code as printed in Amiga magazines. It's a one-pass assembler, with various interesting limitations:

* produces executables directly, no linker needed
* one level of #include nesting.
* limited instruction set support (see the first lines).
* limited forward references
* limited arithmetic support in operands
* limited structure support

And yes, reading the code will cause a SAN check - I think there's like 6 different passes.

--
Tassos Bassoukos
