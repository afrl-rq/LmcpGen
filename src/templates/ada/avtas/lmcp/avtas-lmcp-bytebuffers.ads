with System;                use System;
with AVTAS.LMCP.Types;      use AVTAS.LMCP.Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package AVTAS.LMCP.ByteBuffers is

--  static const int64_t SeriesId;

   Maximum_Length : constant := UInt32'Last - 1;

   subtype Index is UInt32 range 1 .. Maximum_Length;

   type ByteBuffer (Capacity : Index) is tagged private;
   --  ByteBuffers are used either to process incoming messages, or to process
   --  outgoing messages, but not both at the same time. Therefore, Position()
   --  is always the next place within the buffer for reading, or for writing,
   --  but not for both.

   procedure Rewind (This : in out ByteBuffer) with
     Post'Class => Position (This) = 1;

   procedure Clear (This : in out ByteBuffer) with
     Post'Class => Position (This) = 1;

   function Remaining (This : ByteBuffer) return UInt32;

   function Has_Remaining (This : ByteBuffer) return Boolean;

   function Position (This : ByteBuffer) return UInt32;

   --procedure setPosition(this : in out ByteBuffer; position : in Nat);
   --procedure incrementPosition(this : in out ByteBuffer; amount : in Int32);

   --  We use procedures rather than functions for the sake of SPARK, if proof
   --  is to be applied

   procedure Get_Byte (This : in out ByteBuffer;  Value : out Byte) with
     Pre'Class  => Remaining (This) >= 1,
     Post'Class => Position (This) = Position (This)'Old + 1;

--  ByteBuffer & get(uint8_t* dst, uint32_t length, uint32_t offset = 0);

   procedure Get_Boolean (This : in out ByteBuffer;  Value : out Boolean) with
     Pre'Class  => Remaining (This) >= 1,
     Post'Class => Position (This) = Position (This)'Old + 1;

   procedure Get_Int16 (This : in out ByteBuffer;  Value : out Int16) with
     Pre'Class  => Remaining (This) >= 2,
     Post'Class => Position (This) = Position (This)'Old + 2;

   procedure Get_Short (This : in out ByteBuffer;  Value : out Int16) renames Get_Int16;

   procedure Get_UInt16 (This : in out ByteBuffer;  Value : out UInt16) with
     Pre'Class  => Remaining (This) >= 2,
     Post'Class => Position (This) = Position (This)'Old + 2;

   procedure Get_UShort (This : in out ByteBuffer;  Value : out UInt16) renames Get_UInt16;

   procedure Get_Int32 (This : in out ByteBuffer;  Value : out Int32) with
     Pre'Class  => Remaining (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4;

   procedure Get_Int (This : in out ByteBuffer;  Value : out Int32) renames Get_Int32;

   procedure Get_UInt32 (This : in out ByteBuffer;  Value : out UInt32) with
     Pre'Class  => Remaining (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4;

   procedure Get_UInt (This : in out ByteBuffer;  Value : out UInt32) renames Get_UInt32;

   procedure Get_Int64  (This : in out ByteBuffer;  Value : out Int64) with
     Pre'Class  => Remaining (This) >= 8,
     Post'Class => Position (This) = Position (This)'Old + 8;

   procedure Get_Long (This : in out ByteBuffer;  Value : out Int64) renames Get_Int64;

   procedure Get_UInt64 (This : in out ByteBuffer;  Value : out UInt64) with
      Pre'Class  => Remaining (This) >= 8,
      Post'Class => Position (This) = Position (This)'Old + 8;

   procedure Get_ULong (This : in out ByteBuffer;  Value : out UInt64) renames Get_UInt64;

   procedure Get_Real32 (This : in out ByteBuffer;  Value : out Real32) with
     Pre'Class  => Remaining (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4;

   procedure Get_Real64 (This : in out ByteBuffer;  Value : out Real64) with
     Pre'Class  => Remaining (This) >= 8,
     Post'Class => Position (This) = Position (This)'Old + 8;

   --  std::string getString(void);
   procedure Get_String
     (This  : in out ByteBuffer;
      Value : out String;
      Last  : out Natural)
     with
       Pre'Class  => Remaining (This) >= 3,
       Post'Class => Last in Value'Range;
   --  The string content is preceeded in the buffer by a two-byte length, and
   --  there could be at most one actual character in the string (ie, a length
   --  of 1), so we check that there are least three bytes available. The
   --  corresponding Put routine does not allow inserting an empty string.

   procedure Get_Unbounded_String
     (This  : in out ByteBuffer;
      Value : out Unbounded_String);

   --  ByteBuffer & put(const uint8_t b);
   procedure Put_Byte (This : in out ByteBuffer;  Value : Byte) with
     Pre'Class => Remaining (This) >= 1;

--  ByteBuffer & put(const uint8_t * src, uint32_t length, uint32_t offset = 0);

--  ByteBuffer & put(ByteBuffer & buf);

   --  ByteBuffer & putBool(bool b);
   procedure Put_Boolean (This : in out ByteBuffer;  Value : Boolean) with
     Pre'Class => Remaining (This) >= 1;

   --  ByteBuffer & putShort(int16_t s);
   procedure Put_Int16 (This : in out ByteBuffer;  Value : Int16) with
     Pre'Class => Remaining (This) >= 2;

   procedure Put_Short (This : in out ByteBuffer;  Value : Int16) renames Put_Int16;

   --  ByteBuffer & putUShort(uint16_t us);
   procedure Put_UInt16 (This : in out ByteBuffer;  Value : UInt16) with
     Pre'Class => Remaining (This) >= 2;

   procedure Put_UShort (This : in out ByteBuffer;  Value : UInt16) renames Put_UInt16;

   --  ByteBuffer & putInt(int32_t i);
   procedure Put_Int32 (This : in out ByteBuffer;  Value : Int32) with
     Pre'Class => Remaining (This) >= 4;

   procedure Put_Int (This : in out ByteBuffer;  Value : Int32) renames Put_Int32;

   --  ByteBuffer & putUInt(uint32_t ui);
   procedure Put_UInt32 (This : in out ByteBuffer;  Value : UInt32) with
     Pre'Class => Remaining (This) >= 4;

   procedure Put_UInt (This : in out ByteBuffer;  Value : UInt32) renames Put_UInt32;

   --  ByteBuffer & putLong(int64_t l);
   procedure Put_Int64 (This : in out ByteBuffer;  Value : Int64) with
     Pre'Class => Remaining (This) >= 8;
   procedure Put_Long (This : in out ByteBuffer;  Value : Int64) renames Put_Int64;

   --  ByteBuffer & putULong(uint64_t ul);
   procedure Put_UInt64 (This : in out ByteBuffer;  Value : UInt64) with
     Pre'Class => Remaining (This) >= 8;
--
   procedure Put_ULong (This : in out ByteBuffer;  Value : UInt64) renames Put_UInt64;

   --  ByteBuffer & putFloat(float f);
   procedure Put_Real32 (This : in out ByteBuffer;  Value : Real32) with
     Pre'Class => Remaining (This) >= 4;

   --  ByteBuffer & putDouble(double d);
   procedure Put_Real64 (This : in out ByteBuffer;  Value : Real64) with
     Pre'Class => Remaining (This) >= 8;

   --  ByteBuffer & putString(std::string s);
   procedure Put_String (This : in out ByteBuffer;  Value : String) with
     Pre'Class => Value /= "" and Remaining (This) >= Value'Length + 2;  -- 2 bytes for the length

   -- Populate the ByteBuffer from the bytes in a String. Useful for then
   -- rewinding and reading back out meaningful objects.
   procedure Put_Raw_Bytes (This : in out ByteBuffer; Value : String) with
     Pre'Class => Value /= "" and Remaining (This) >= Value'Length; -- we don't write the length

   procedure Put_Unbounded_String (This : in out ByteBuffer; Value : Unbounded_String);

   type Byte_Array is array (Index range <>) of Byte
     with Component_Size => 1 * Storage_Unit;   -- confirming

   function Raw_Bytes (This : ByteBuffer) return Byte_Array;
   function Raw_Bytes (This : ByteBuffer) return String;

--     type Int16_Array   is array (Index range <>) of Int16_T  with Component_Size => 2 * Storage_Unit;
--     type Int32_Array   is array (Index range <>) of Int32_t  with Component_Size => 4 * Storage_Unit;
--     type Int64_Array   is array (Index range <>) of Int64_t  with Component_Size => 8 * Storage_Unit;
--     type UInt16_Array  is array (Index range <>) of UInt16_t with Component_Size => 2 * Storage_Unit;
--     type UInt32_Array  is array (Index range <>) of UInt32_t with Component_Size => 4 * Storage_Unit;
--     type Float_Array   is array (Index range <>) of Float_t  with Component_Size => 4 * Storage_Unit;
--     type Double_Array  is array (Index range <>) of Double_t with Component_Size => 8 * Storage_Unit;
--     type Boolean_Array is array (Index range <>) of Boolean  with Component_Size => 1 * Storage_Unit;
--     type StringArray  is array (Index range <>) of Unbounded_String;  -- or Dynamic_String?

--  ByteBuffer & putBoolArray(const bool * b, uint32_t length);
--  ByteBuffer & putByteArray(const uint8_t * b, uint32_t length);
--  ByteBuffer & putShortArray(const int16_t * s, uint32_t length);
--  ByteBuffer & putUShortArray(const uint16_t * us, uint32_t length);
--  ByteBuffer & putIntArray(const int32_t * i, uint32_t length);
--  ByteBuffer & putUIntArray(const uint32_t * ui, uint32_t length);
--  ByteBuffer & putLongArray(const int64_t * l, uint32_t length);
--  ByteBuffer & putULongArray(const uint64_t * ul, uint32_t length);
--  ByteBuffer & putFloatArray(const float * f, uint32_t length);
--  ByteBuffer & putDoubleArray(const double * d, uint32_t length);

--  ByteBuffer & getBoolArray(bool * b, uint32_t length);
--  ByteBuffer & getByteArray(uint8_t * c, uint32_t length);
--  ByteBuffer & getShortArray(int16_t * s, uint32_t length);
--  ByteBuffer & getUShortArray(uint16_t * us, uint32_t length);
--  ByteBuffer & getIntArray(int32_t * i, uint32_t length);
--  ByteBuffer & getUIntArray(uint32_t * ui, uint32_t length);
--  ByteBuffer & getLongArray(int64_t * l, uint32_t length);
--  ByteBuffer & getULongArray(uint64_t * ul, uint32_t length);
--  ByteBuffer & getFloatArray(float * f, uint32_t length);
--  ByteBuffer & getDoubleArray(double * d, uint32_t length);

private

   type ByteBuffer (Capacity : Index) is tagged record
      Buffer   : Byte_Array (1 .. Capacity) := (others => 0);
      Position : Index := 1;
   end record;

end AVTAS.LMCP.ByteBuffers;
