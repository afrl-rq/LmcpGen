with avtas.lmcp.types; use avtas.lmcp.types;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package avtas.lmcp.byteBuffers is
   
   subtype Nat is UInt32_t range 1 .. UInt32_t'Last;
   type ByteArray is array (Nat range <>) of Byte;
   subtype ByteArray2 is ByteArray(1 .. 2);
   subtype ByteArray4 is ByteArray(1 .. 4);
   subtype ByteArray8 is ByteArray(1 .. 8);
   
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
   
--     function To_Boolean(this : ByteBuffer) return Boolean;
--     function To_Int16(this : ByteBuffer) return Int16_t;
--     function To_Int32(this : ByteBuffer) return Int32_t;
--     function To_Int64(this : ByteBuffer) return Int64_t;
--     function To_UInt16(this : ByteBuffer) return UInt16_t;
--     function To_UInt32(this : ByteBuffer) return UInt32_t;
--     function To_Float(this : ByteBuffer) return Float_t;
--     function To_Double(this : ByteBuffer) return Double_t;
--     function To_String(this : ByteBuffer; numBytes : UInt32_t) return Unbounded_String;
   
   procedure Get_Boolean(this : in out ByteBuffer; output : out Boolean);
   procedure Get_Byte(this : in out ByteBuffer; output : out Byte);
   procedure Get_Int16_t(this : in out ByteBuffer; output : out Int16_t);
   procedure Get_Int32_t(this : in out ByteBuffer; output : out Int32_t);
   procedure Get_Int64_t(this : in out ByteBuffer; output : out Int64_t);
   procedure Get_UInt16_t(this : in out ByteBuffer; output : out UInt16_t);
   procedure Get_UInt32_t(this : in out ByteBuffer; output : out UInt32_t);
   procedure Get_Float_t(this : in out ByteBuffer; output : out Float_t);
   procedure Get_Double_t(this : in out ByteBuffer; output : out Double_t);
   procedure Get_Unbounded_String(this : in out ByteBuffer; output : out Unbounded_String);
   
   procedure Put_Boolean(input : in Boolean; this : in out ByteBuffer);
   procedure Put_Byte(input : in Byte; this : in out ByteBuffer);
   procedure Put_Int16_t(input : in Int16_t; this :  in out ByteBuffer);
   procedure Put_Int32_t(input : in Int32_t; this :  in out ByteBuffer);
   procedure Put_Int64_t(input : in Int64_t; this :  in out ByteBuffer);
   procedure Put_UInt16_t(input : in UInt16_t; this :  in out ByteBuffer);
   procedure Put_UInt32_t(input : in UInt32_t; this :  in out ByteBuffer);
   procedure Put_Float_t(input : in Float_t; this :  in out ByteBuffer);
   procedure Put_Double_t(input : in Double_t; this :  in out ByteBuffer);
   procedure Put_Unbounded_String(input : in Unbounded_String; this : in out ByteBuffer);
   
--     function To_BooleanArray(this : ByteBuffer; numBytes : UInt32_t) return BooleanArray;
--     function To_StringArray(this : ByteBuffer; numBytes : UInt32_t) return StringArray;
--     function To_Int16Array(this : ByteBuffer; numBytes : UInt32_t) return Int16Array;
--     function To_Int32Array(this : ByteBuffer; numBytes : UInt32_t) return Int32Array;
--     function To_Int64Array(this : ByteBuffer; numBytes : UInt32_t) return Int64Array;
--     function To_UInt16Array(this : ByteBuffer; numBytes : UInt32_t) return UInt16Array;
--     function To_UInt32Array(this : ByteBuffer; numBytes : UInt32_t) return UInt32Array;
--     function To_FloatArray(this : ByteBuffer; numBytes : UInt32_t) return FloatArray;
--     function To_DoubleArray(this : ByteBuffer; numBytes : UInt32_t) return DoubleArray;

--   procedure Get_BooleanArray(this : in out ByteBuffer; output: out BooleanArray; isLarge : in Boolean);
--   procedure Get_StringArray(this : in out ByteBuffer; output: out StringArray; isLarge : in Boolean);
--   procedure Get_Int16Array(this : in out ByteBuffer; output: out Int16Array; isLarge : in Boolean);
--   procedure Get_Int32Array(this : in out ByteBuffer; output : out Int32Array; isLarge : in Boolean);
--   procedure Get_Int64Array(this : in out ByteBuffer; output : out Int64Array; isLarge : in Boolean);
--   procedure Get_UInt16Array(this : in out ByteBuffer; output : out UInt16Array; isLarge : in Boolean);
--   procedure Get_UInt32Array(this : in out ByteBuffer; output : out UInt32Array; isLarge : in Boolean);
--   procedure Get_FloatArray(this : in out ByteBuffer; output : out FloatArray; isLarge : in Boolean);
--   procedure Get_DoubleArray(this : in out ByteBuffer; output : out DoubleArray; isLarge : in Boolean);
   
   -- Turns out LMCP assumes big endian, whereas these unchecked conversions assume
   -- little endian. So it is necessary to swap bytes first before doing the unchecked conversion.
   
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
   
   function capacity(this : ByteBuffer) return Nat is (this.Capacity);
   function getPosition(this : ByteBuffer) return Nat is (this.Pos);
   function remaining(this : ByteBuffer) return UInt32_t is (UInt32_t(this.Capacity - this.Pos));
   function hasRemaining(this : ByteBuffer) return Boolean is (this.Pos < this.Capacity);
   
   procedure setPosition(this : in out ByteBuffer; position : in Nat);
   procedure incrementPosition(this : in out ByteBuffer; amount : in UInt32_t);
   
end avtas.lmcp.byteBuffers;
