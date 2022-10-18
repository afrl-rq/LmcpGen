LmcpGen/src/templates/ada
=========================

Ada support for LmcpGen.

Includes a fully proven SPARK implementation of byte buffers, contained in `avtas/lmcp`.
To prove the code:

    lmcpgen/src/templates/ada/avtas $ gnatprove -P test_bytebuffers.gpr

All checks pass in SPARK Pro 22.2; two checks are unproven in SPARK Community 21.
