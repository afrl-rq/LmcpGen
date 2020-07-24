with avtas.lmcp.types; use avtas.lmcp.types;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package avtas.lmcp.byteBuffers is
   
   type Nat is range 1 .. UInt32_t'Last;
   type ByteArray is array (Nat range <>) of Byte;
   type ByteArray2 is array (1 .. 2) of Byte;
   type ByteArray4 is array (1 .. 4) of Byte;
   type ByteArray8 is array (1 .. 8) of Byte;
   
   type Int16Array is array (Nat range <>) of Int16_t;
   type Int32Array is array (Nat range <>) of Int32_t;
   type Int64Array is array (Nat range <>) of Int64_t;
   type UInt16Array is array (Nat range <>) of UInt16_t;
   type UInt32Array is array (Nat range <>) of UInt32_t;
   type FloatArray is array (Nat range <>) of Float_t;
   type DoubleArray is array (Nat range <>) of Double_t;
   type StringArray is array (Nat range <>) of Unbounded_String;
   type BooleanArray is array (Nat range <>) of Boolean;
   
   type ByteBuffer(Capacity : Nat) is record 
      Pos : Nat := 1;
      Buf : ByteArray(1 .. Capacity);
   end record;
   
   function To_Boolean(Source : Byte) return Boolean is (if Source = 0 then False else True);
   function To_String(Input : ByteArray) return Unbounded_String;
   function To_Int16(Input : ByteArray2) return Int16_t;
   function To_Int32(Input : ByteArray4) return Int32_t;
   function To_Int64(Input : ByteArray8) return Int64_t;
   function To_UInt16(Input : ByteArray2) return UInt16_t;
   function To_UInt32(Input : ByteArray4) return UInt32_t;
   function To_Float(Input : ByteArray4) return Float_t;
   function To_Double(Input : ByteArray8) return Double_t;
   
--     function To_Int16 is
--       new Ada.Unchecked_Conversion (Source => ByteArray2, Target => Int16_t);
--     function To_Int32 is
--       new Ada.Unchecked_Conversion (Source => ByteArray4, Target => Int32_t);
--     function To_Int64 is
--       new Ada.Unchecked_Conversion (Source => ByteArray8, Target => Int64_t);
--     function To_UInt16 is
--       new Ada.Unchecked_Conversion (Source => ByteArray2, Target => UInt16_t);
--     function To_UInt32 is
--       new Ada.Unchecked_Conversion (Source => ByteArray4, Target => UInt32_t);
--     function To_Float is
--       new Ada.Unchecked_Conversion (Source => ByteArray4, Target => Float_t);
--     function To_Double is
--       new Ada.Unchecked_Conversion (Source => ByteArray8, Target => Double_t);
   
   function To_BooleanArray(Input : ByteArray) return BooleanArray;
   function To_StringArray(Input : ByteArray) return StringArray;
   function To_Int16Array(Input : ByteArray) return Int16Array;
   function To_Int32Array(Input : ByteArray) return Int32Array;
   function To_Int64Array(Input : ByteArray) return Int64Array;
   function To_UInt16Array(Input : ByteArray) return UInt16Array;
   function To_UInt32Array(Input : ByteArray) return UInt32Array;
   function To_FloatArray(Input : ByteArray) return FloatArray;
   function To_DoubleArray(Input : ByteArray) return DoubleArray;
   
   function capacity(this : ByteBuffer) return Nat is (this.Capacity);
   function getPosition(this : ByteBuffer) return Nat is (this.Pos);
   function remaining(this : ByteBuffer) return UInt32_t is (UInt32_t(this.Capacity - this.Pos));
   function hasRemaining(this : ByteBuffer) return Boolean is (this.Pos < this.Capacity);
   
   --procedure setPosition(this : in out ByteBuffer; position : in Nat);
   --procedure incrementPosition(this : in out ByteBuffer; amount : in Int32_t);
   
end avtas.lmcp.byteBuffers;
