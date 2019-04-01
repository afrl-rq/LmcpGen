with System;                use System;
with AVTAS.LMCP.Types;      use AVTAS.LMCP.Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package AVTAS.LMCP.ByteBuffers is

   Maximum_Length : constant := UInt32'Last - 1;

   subtype Index is UInt32 range 1 .. Maximum_Length;

   type ByteBuffer (Capacity : Index) is tagged private with
     Default_Initial_Condition =>
       Position (ByteBuffer) = 1 and
       Length (ByteBuffer) = 0;

   procedure Rewind (This : in out ByteBuffer) with
     Post'Class => Position (This) = 1 and
                   Length (This) = Length (This)'Old;
   --  For reading, resetting the buffer position would allow reading of
   --  existing content from the beginning, presumably after writing
   --  values into it (via the Put_* routines).
   --
   --  For writing, resetting the buffer position would make subsequent calls
   --  to Put_* start over at the beginning, overwriting any existing values,
   --  but NOTE that this does not reset the length, i.e., new Put_* calls
   --  would increase the Length determined by any Put_* calls made prior
   --  to Rewind. As a result, for writing you probably want to call Clear
   --  instead.

   procedure Clear (This : in out ByteBuffer) with
     Post'Class => Position (This) = 1 and
                   Length (This) = 0;

   function Remaining (This : ByteBuffer) return UInt32;

   function Has_Remaining (This : ByteBuffer) return Boolean with
     Post => ((Has_Remaining'Result) = (Remaining (This) > 0));

   function Position (This : ByteBuffer) return UInt32;
   --  Position always returns the next place within the buffer for reading or
   --  for writing.
   --
   --  Both Put_* and Get_* routines affect the value of Position. Only the
   --  Put_* routines affect the Length.

   function Length (This : ByteBuffer) return UInt32;
   --  The logical length of the content of the buffer, i.e., the "high water
   --  mark" for values placed into the buffer by the various Put_* routines.
   --  Not affected by the Get_* routines. The logical length of the content
   --  may not be equal the Capacity.

  --  We use procedures rather than functions for the sake of SPARK, if proof
   --  is to be applied

   procedure Get_Byte (This : in out ByteBuffer;  Value : out Byte) with
     Pre'Class  => Remaining (This) >= 1,
     Post'Class => Position (This) = Position (This)'Old + 1 and
                   Length (This) = Length (This)'Old;

   procedure Get_Boolean (This : in out ByteBuffer;  Value : out Boolean) with
     Pre'Class  => Remaining (This) >= 1,
     Post'Class => Position (This) = Position (This)'Old + 1 and
                   Length (This) = Length (This)'Old;

   procedure Get_Int16 (This : in out ByteBuffer;  Value : out Int16) with
     Pre'Class  => Remaining (This) >= 2,
     Post'Class => Position (This) = Position (This)'Old + 2 and
                   Length (This) = Length (This)'Old;

   procedure Get_Short (This : in out ByteBuffer;  Value : out Int16) renames Get_Int16;

   procedure Get_UInt16 (This : in out ByteBuffer;  Value : out UInt16) with
     Pre'Class  => Remaining (This) >= 2,
     Post'Class => Position (This) = Position (This)'Old + 2 and
                   Length (This) = Length (This)'Old;

   procedure Get_UShort (This : in out ByteBuffer;  Value : out UInt16) renames Get_UInt16;

   procedure Get_Int32 (This : in out ByteBuffer;  Value : out Int32) with
     Pre'Class  => Remaining (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4 and
                   Length (This) = Length (This)'Old;

   procedure Get_Int (This : in out ByteBuffer;  Value : out Int32) renames Get_Int32;

   procedure Get_UInt32
     (This  : in ByteBuffer;
      Value : out UInt32;
      First : Index)
   with
     Pre'Class  => First <= Length (This) - 3,
     Post'Class => Position (This) = Position (This)'Old and
                   Length (This) = Length (This)'Old;
   --  Gets a UInt32 value from This buffer, at indexes First .. First + 3
   --  rather than from This.Position .. This.Positon + 3

   procedure Get_UInt32 (This : in out ByteBuffer;  Value : out UInt32) with
     Pre'Class  => Remaining (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4 and
                   Length (This) = Length (This)'Old;

   procedure Get_UInt (This : in out ByteBuffer;  Value : out UInt32) renames Get_UInt32;

   procedure Get_Int64  (This : in out ByteBuffer;  Value : out Int64) with
     Pre'Class  => Remaining (This) >= 8,
     Post'Class => Position (This) = Position (This)'Old + 8 and
                   Length (This) = Length (This)'Old;

   procedure Get_Long (This : in out ByteBuffer;  Value : out Int64) renames Get_Int64;

   procedure Get_UInt64 (This : in out ByteBuffer;  Value : out UInt64) with
      Pre'Class  => Remaining (This) >= 8,
      Post'Class => Position (This) = Position (This)'Old + 8 and
                    Length (This) = Length (This)'Old;

   procedure Get_ULong (This : in out ByteBuffer;  Value : out UInt64) renames Get_UInt64;

   procedure Get_Real32 (This : in out ByteBuffer;  Value : out Real32) with
     Pre'Class  => Remaining (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4 and
                   Length (This) = Length (This)'Old;

   procedure Get_Real64 (This : in out ByteBuffer;  Value : out Real64) with
     Pre'Class  => Remaining (This) >= 8,
     Post'Class => Position (This) = Position (This)'Old + 8 and
                   Length (This) = Length (This)'Old;

   procedure Get_String
     (This  : in out ByteBuffer;
      Value : out String;
      Last  : out Natural)
   with
     Pre'Class  => Remaining (This) >= 2,
     Post'Class =>
       --  Last is Value'First - 1 when the number of characters is read as zero, otherwise it is in Value'Range
       Last in Value'First - 1 .. Value'Last and
       --  we read the length, which was zero, so nothing else was read
       (if Last = Value'First - 1  then Position (This) = Position (This'Old) + 2) and
       --  we read the length, which was nonzero, and then that many characters
       (if Last /= Value'First - 1 then Position (This) = Position (This'Old) + 2 + UInt32(Last - Value'First + 1)) and
       Length (This) = Length (This)'Old;
   --  The string content is preceeded in the buffer by a two-byte length,
   --  even when zero, so the precondition checks that there are least
   --  that many bytes available.

   procedure Get_Unbounded_String
     (This  : in out ByteBuffer;
      Value : out Unbounded_String)
   with
     Pre'Class  => Remaining (This) >= 3,
     Post'Class =>
       --  we read the length, which was zero, so nothing else was read
       (if Value = Null_Unbounded_String  then Position (This) = Position (This'Old) + 2) and
       --  we read the length, which was nonzero, and then that many characters
       (if Value /= Null_Unbounded_String then Position (This) = Position (This'Old) + 2 + UInt32 (Length (Value))) and
       Length (This) = Length (This)'Old;

   procedure Put_Byte (This : in out ByteBuffer;  Value : Byte) with
     Pre'Class  => Remaining (This) >= 1,
     Post'Class => Position (This) = Position (This)'Old + 1 and
                   Length (This) = Length (This)'Old + 1;

   procedure Put_Boolean (This : in out ByteBuffer;  Value : Boolean) with
     Pre'Class  => Remaining (This) >= 1,
     Post'Class => Position (This) = Position (This)'Old + 1 and
                   Length (This) = Length (This)'Old + 1;

   procedure Put_Int16 (This : in out ByteBuffer;  Value : Int16) with
     Pre'Class  => Remaining (This) >= 2,
     Post'Class => Position (This) = Position (This)'Old + 2 and
                   Length (This) = Length (This)'Old + 2;

   procedure Put_Short (This : in out ByteBuffer;  Value : Int16) renames Put_Int16;

   procedure Put_UInt16 (This : in out ByteBuffer;  Value : UInt16) with
     Pre'Class  => Remaining (This) >= 2,
     Post'Class => Position (This) = Position (This)'Old + 2 and
                   Length (This) = Length (This)'Old + 2;

   procedure Put_UShort (This : in out ByteBuffer;  Value : UInt16) renames Put_UInt16;

   procedure Put_Int32 (This : in out ByteBuffer;  Value : Int32) with
     Pre'Class  => Remaining (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4 and
                   Length (This) = Length (This)'Old + 4;

   procedure Put_Int (This : in out ByteBuffer;  Value : Int32) renames Put_Int32;

   procedure Put_UInt32 (This : in out ByteBuffer;  Value : UInt32) with
     Pre'Class  => Remaining (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4 and
                   Length (This) = Length (This)'Old + 4;

   procedure Put_UInt (This : in out ByteBuffer;  Value : UInt32) renames Put_UInt32;

   procedure Put_Int64 (This : in out ByteBuffer;  Value : Int64) with
     Pre'Class  => Remaining (This) >= 8,
     Post'Class => Position (This) = Position (This)'Old + 8 and
                   Length (This) = Length (This)'Old + 8;

   procedure Put_Long (This : in out ByteBuffer;  Value : Int64) renames Put_Int64;

   procedure Put_UInt64 (This : in out ByteBuffer;  Value : UInt64) with
     Pre'Class  => Remaining (This) >= 8,
     Post'Class => Position (This) = Position (This)'Old + 8 and
                   Length (This) = Length (This)'Old + 8;

   procedure Put_ULong (This : in out ByteBuffer;  Value : UInt64) renames Put_UInt64;

   --  ByteBuffer & putFloat(float f);
   procedure Put_Real32 (This : in out ByteBuffer;  Value : Real32) with
     Pre'Class  => Remaining (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4 and
                   Length (This) = Length (This)'Old + 4;

   --  ByteBuffer & putDouble(double d);
   procedure Put_Real64 (This : in out ByteBuffer;  Value : Real64) with
     Pre'Class  => Remaining (This) >= 8,
     Post'Class => Position (This) = Position (This)'Old + 8 and
                   Length (This) = Length (This)'Old + 8;

   procedure Put_String (This : in out ByteBuffer;  Value : String) with
     Pre'Class  => Remaining (This) >= Value'Length + 2,  -- 2 bytes for the length
     Post'Class => Position (This) = Position (This)'Old + 2 + Value'Length and
                   Length (This) = Length (This)'Old + Value'Length + 2;

   -- Populate the ByteBuffer from the bytes in a String. Useful for then
   -- rewinding and reading back out meaningful objects. The input Value is a
   -- String because that's the most convenient choice, based on client usage.
   procedure Put_Raw_Bytes (This : in out ByteBuffer; Value : String) with
     Pre'Class  => Value /= "" and Remaining (This) >= Value'Length, -- we don't write the length attr
     Post'Class =>
       Position (This) = Position (This)'Old + Value'Length and -- we don't write the length attr
       Length (This) = Length (This)'Old + Value'Length; -- we don't write the length attr

   procedure Put_Unbounded_String (This : in out ByteBuffer; Value : Unbounded_String) with
     Pre'Class  => Integer (Remaining (This)) >= 2 + Length (Value),  -- 2 bytes for the length
     Post'Class =>
       Position (This) = Position (This)'Old + 2 + UInt32 (Length (Value)) and
       Length (This) = Length (This)'Old + 2 + UInt32 (Length (Value));

   type Byte_Array is array (Index range <>) of Byte
     with Component_Size => 1 * Storage_Unit;   -- confirming

   function Raw_Bytes (This : ByteBuffer) return Byte_Array;
   --  Returns the full internal byte array content

   function Raw_Bytes (This : ByteBuffer) return String;
   --  Returns the full internal byte array content, as a String

   function Checksum (This : ByteBuffer;  From, To : Index) return UInt32 with
     Pre'Class =>
       From <= To                and   -- null ranges are not useful
       From <= This.Capacity     and   -- physically possible
       To   <= This.Capacity     and
       From <= This.Length       and   -- logically possible
       To   <= This.Length;
   --  Computes the checksum of the slice of the internal byte array From .. To.

private

   subtype Natural_Index is UInt32 range 0 .. Maximum_Length;

   type ByteBuffer (Capacity : Index) is tagged record
      Content  : Byte_Array (1 .. Capacity) := (others => 0);
      Position : Index := 1;    -- reset to 1 by Rewind
      Length   : Natural_Index := 0;   -- reset to by Clear
   end record;

end AVTAS.LMCP.ByteBuffers;
