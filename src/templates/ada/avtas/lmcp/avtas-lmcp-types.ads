with Interfaces; use Interfaces;

package AVTAS.LMCP.Types is

   -- C/C++ compatible integer types
   type Byte   is new Interfaces.Unsigned_8;
   type UInt16 is new Interfaces.Unsigned_16;
   type UInt32 is new Interfaces.Unsigned_32;
   type UInt64 is new Interfaces.Unsigned_64;
   type Int16  is new Interfaces.Integer_16;
   type Int32  is new Interfaces.Integer_32;
   type Int64  is new Interfaces.Integer_64;
   type Real32 is new Interfaces.IEEE_Float_32;
   type Real64 is new Interfaces.IEEE_Float_64;

end AVTAS.LMCP.Types;
