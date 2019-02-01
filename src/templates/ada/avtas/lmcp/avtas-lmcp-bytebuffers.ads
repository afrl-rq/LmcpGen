with avtas.lmcp.types; use avtas.lmcp.types;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package avtas.lmcp.byteBuffers is
   
   subtype Nat is UInt32 range 1 .. UInt32'Last;
   type ByteArray is array (Nat range <>) of Byte;
   subtype ByteArray2 is ByteArray(1 .. 2);
   subtype ByteArray4 is ByteArray(1 .. 4);
   subtype ByteArray8 is ByteArray(1 .. 8);
   
   type Int16Array is array (Nat range <>) of Int16;
   type Int32Array is array (Nat range <>) of Int32;
   type Int64Array is array (Nat range <>) of Int64;
   type UInt16Array is array (Nat range <>) of UInt16;
   type UInt32Array is array (Nat range <>) of UInt32;
   type FloatArray is array (Nat range <>) of Real32;
   type DoubleArray is array (Nat range <>) of Real64;
   type StringArray is array (Nat range <>) of Unbounded_String;
   type BooleanArray is array (Nat range <>) of Boolean;
   
   type ByteBuffer(Capacity : Nat) is record 
      Pos : Nat := 1;
      Buf : ByteArray(1 .. Capacity);
   end record;
   
--     function To_Boolean(this : ByteBuffer) return Boolean;
--     function To_Int16(this : ByteBuffer) return Int16;
--     function To_Int32(this : ByteBuffer) return Int32;
--     function To_Int64(this : ByteBuffer) return Int64;
--     function To_UInt16(this : ByteBuffer) return UInt16;
--     function To_UInt32(this : ByteBuffer) return UInt32;
--     function To_Float(this : ByteBuffer) return Real32;
--     function To_Double(this : ByteBuffer) return Real64;
--     function To_String(this : ByteBuffer; numBytes : UInt32) return Unbounded_String;
   
   procedure Get_Boolean(this : in out ByteBuffer; output : out Boolean);
   procedure Get_Byte(this : in out ByteBuffer; output : out Byte);
   procedure Get_Int16(this : in out ByteBuffer; output : out Int16);
   procedure Get_Int32(this : in out ByteBuffer; output : out Int32);
   procedure Get_Int64(this : in out ByteBuffer; output : out Int64);
   procedure Get_UInt16(this : in out ByteBuffer; output : out UInt16);
   procedure Get_UInt32(this : in out ByteBuffer; output : out UInt32);
   procedure Get_Real32(this : in out ByteBuffer; output : out Real32);
   procedure Get_Real64(this : in out ByteBuffer; output : out Real64);
   procedure Get_Unbounded_String(this : in out ByteBuffer; output : out Unbounded_String);
   
   procedure Put_Boolean(input : in Boolean; this : in out ByteBuffer);
   procedure Put_Byte(input : in Byte; this : in out ByteBuffer);
   procedure Put_Int16(input : in Int16; this :  in out ByteBuffer);
   procedure Put_Int32(input : in Int32; this :  in out ByteBuffer);
   procedure Put_Int64(input : in Int64; this :  in out ByteBuffer);
   procedure Put_UInt16(input : in UInt16; this :  in out ByteBuffer);
   procedure Put_UInt32(input : in UInt32; this :  in out ByteBuffer);
   procedure Put_Real32(input : in Real32; this :  in out ByteBuffer);
   procedure Put_Real64(input : in Real64; this :  in out ByteBuffer);
   procedure Put_Unbounded_String(input : in Unbounded_String; this : in out ByteBuffer);
   
--     function To_BooleanArray(this : ByteBuffer; numBytes : UInt32) return BooleanArray;
--     function To_StringArray(this : ByteBuffer; numBytes : UInt32) return StringArray;
--     function To_Int16Array(this : ByteBuffer; numBytes : UInt32) return Int16Array;
--     function To_Int32Array(this : ByteBuffer; numBytes : UInt32) return Int32Array;
--     function To_Int64Array(this : ByteBuffer; numBytes : UInt32) return Int64Array;
--     function To_UInt16Array(this : ByteBuffer; numBytes : UInt32) return UInt16Array;
--     function To_UInt32Array(this : ByteBuffer; numBytes : UInt32) return UInt32Array;
--     function To_FloatArray(this : ByteBuffer; numBytes : UInt32) return FloatArray;
--     function To_DoubleArray(this : ByteBuffer; numBytes : UInt32) return DoubleArray;

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
--       new Ada.Unchecked_Conversion (Source => ByteArray2, Target => Int16);
--     function To_Int32 is
--       new Ada.Unchecked_Conversion (Source => ByteArray4, Target => Int32);
--     function To_Int64 is
--       new Ada.Unchecked_Conversion (Source => ByteArray8, Target => Int64);
--     function To_UInt16 is
--       new Ada.Unchecked_Conversion (Source => ByteArray2, Target => UInt16);
--     function To_UInt32 is
--       new Ada.Unchecked_Conversion (Source => ByteArray4, Target => UInt32);
--     function To_Float is
--       new Ada.Unchecked_Conversion (Source => ByteArray4, Target => Real32);
--     function To_Double is
--       new Ada.Unchecked_Conversion (Source => ByteArray8, Target => Real64);
   
   function capacity(this : ByteBuffer) return Nat is (this.Capacity);
   function getPosition(this : ByteBuffer) return Nat is (this.Pos);
   function remaining(this : ByteBuffer) return UInt32 is (UInt32(this.Capacity - this.Pos));
   function hasRemaining(this : ByteBuffer) return Boolean is (this.Pos < this.Capacity);
   
   procedure setPosition(this : in out ByteBuffer; position : in Nat);
   procedure incrementPosition(this : in out ByteBuffer; amount : in UInt32);
   
end avtas.lmcp.byteBuffers;
