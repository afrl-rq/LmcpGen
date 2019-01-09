with Interfaces; use Interfaces;

package avtas.lmcp.types is
   
   -- C/C++ compatible integer types
   type Byte is new Interfaces.Unsigned_8;
   type UInt16_t is new Interfaces.Unsigned_16;
   type UInt32_t is new Interfaces.Unsigned_32;
   type Int16_t is new Interfaces.Integer_16;
   type Int32_t is new Interfaces.Integer_32;
   type Int64_t is new Interfaces.Integer_64;
   type Float_t is new Interfaces.IEEE_Float_32;
   type Double_t is new Interfaces.IEEE_Float_64;

end avtas.lmcp.types;
