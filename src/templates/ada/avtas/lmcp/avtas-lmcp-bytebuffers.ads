with System;                use System;
with AVTAS.LMCP.Types;      use AVTAS.LMCP.Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package AVTAS.LMCP.ByteBuffers is

   pragma Assertion_Policy (Check);
   --  This pragma overrides the effect of the -gnata switch

   Maximum_Length : constant := UInt32'Last - 1;

   subtype Index is UInt32 range 1 .. Maximum_Length;

   type ByteBuffer (Capacity : Index) is tagged private with
     Default_Initial_Condition =>
       Position (ByteBuffer) = 1 and
       Length (ByteBuffer) = 0   and
       Space_Available (ByteBuffer) = Capacity;

   function Space_Available (This : ByteBuffer) return UInt32;
   --  The number of unused bytes physically available remaining in This
   --  buffer. These bytes are not currently allocated to a message. A
   --  function of This.Capacity and Position (This).

   function Msg_Bytes_Remaining (This : ByteBuffer) return UInt32;
   --  The number of bytes allocated to a message that remain as yet unread
   --  (by a call to Get_*) in This buffer. A function of Length (This) and
   --  Position (This).

   function Position (This : ByteBuffer) return UInt32;
   --  Position always returns the next place within the buffer for reading or
   --  for writing.
   --
   --  Both Put_* and Get_* routines affect the value of Position. Only the
   --  Put_* routines affect the Length.

   function Length (This : ByteBuffer) return UInt32;
   --  The number of data bytes currently contained in This buffer, i.e., the
   --  "high water mark" for values placed into the buffer by the various
   --  Put_* routines. Not affected by the Get_* routines (unlike Position).

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

   procedure Get_Byte (This : in out ByteBuffer;  Value : out Byte) with
     Pre'Class  => Msg_Bytes_Remaining (This) >= 1,
     Post'Class => Position (This) = Position (This)'Old + 1 and
                   Length (This) = Length (This)'Old and
                   Msg_Bytes_Remaining (This) = Msg_Bytes_Remaining (This)'Old - 1;

   procedure Get_Boolean (This : in out ByteBuffer;  Value : out Boolean) with
     Pre'Class  => Msg_Bytes_Remaining (This) >= 1,
     Post'Class => Position (This) = Position (This)'Old + 1 and
                   Length (This) = Length (This)'Old and
                   Msg_Bytes_Remaining (This) = Msg_Bytes_Remaining (This)'Old - 1;

   procedure Get_Int16 (This : in out ByteBuffer;  Value : out Int16) with
     Pre'Class  => Msg_Bytes_Remaining (This) >= 2,
     Post'Class => Position (This) = Position (This)'Old + 2 and
                   Length (This) = Length (This)'Old and
                   Msg_Bytes_Remaining (This) = Msg_Bytes_Remaining (This)'Old - 2;

   procedure Get_Short (This : in out ByteBuffer;  Value : out Int16) renames Get_Int16;

   procedure Get_UInt16 (This : in out ByteBuffer;  Value : out UInt16) with
     Pre'Class  => Msg_Bytes_Remaining (This) >= 2,
     Post'Class => Position (This) = Position (This)'Old + 2 and
                   Length (This) = Length (This)'Old and
                   Msg_Bytes_Remaining (This) = Msg_Bytes_Remaining (This)'Old - 2;

   procedure Get_UShort (This : in out ByteBuffer;  Value : out UInt16) renames Get_UInt16;

   procedure Get_Int32 (This : in out ByteBuffer;  Value : out Int32) with
     Pre'Class  => Msg_Bytes_Remaining (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4 and
                   Length (This) = Length (This)'Old and
                   Msg_Bytes_Remaining (This) = Msg_Bytes_Remaining (This)'Old - 4;

   procedure Get_Int (This : in out ByteBuffer;  Value : out Int32) renames Get_Int32;

   procedure Get_UInt32
     (This  : in ByteBuffer;
      Value : out UInt32;
      First : Index)
   with
     Pre'Class  => First <= Length (This) - 3,
     Post'Class => Position (This) = Position (This)'Old and
                   Length (This) = Length (This)'Old and
                   Msg_Bytes_Remaining (This) = Msg_Bytes_Remaining (This)'Old;
   --  Gets a UInt32 value from This buffer, at indexes First .. First + 3
   --  rather than from This.Position .. This.Positon + 3

   procedure Get_UInt32 (This : in out ByteBuffer;  Value : out UInt32) with
     Pre'Class  => Msg_Bytes_Remaining (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4 and
                   Length (This) = Length (This)'Old and
                   Msg_Bytes_Remaining (This) = Msg_Bytes_Remaining (This)'Old - 4;

   procedure Get_UInt (This : in out ByteBuffer;  Value : out UInt32) renames Get_UInt32;

   procedure Get_Int64  (This : in out ByteBuffer;  Value : out Int64) with
     Pre'Class  => Msg_Bytes_Remaining (This) >= 8,
     Post'Class => Position (This) = Position (This)'Old + 8 and
                   Length (This) = Length (This)'Old and
                   Msg_Bytes_Remaining (This) = Msg_Bytes_Remaining (This)'Old - 8;

   procedure Get_Long (This : in out ByteBuffer;  Value : out Int64) renames Get_Int64;

   procedure Get_UInt64 (This : in out ByteBuffer;  Value : out UInt64) with
      Pre'Class  => Msg_Bytes_Remaining (This) >= 8,
      Post'Class => Position (This) = Position (This)'Old + 8 and
                    Length (This) = Length (This)'Old and
                    Msg_Bytes_Remaining (This) = Msg_Bytes_Remaining (This)'Old - 8;

   procedure Get_ULong (This : in out ByteBuffer;  Value : out UInt64) renames Get_UInt64;

   procedure Get_Real32 (This : in out ByteBuffer;  Value : out Real32) with
     Pre'Class  => Msg_Bytes_Remaining (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4 and
                   Length (This) = Length (This)'Old and
                   Msg_Bytes_Remaining (This) = Msg_Bytes_Remaining (This)'Old - 4;

   procedure Get_Real64 (This : in out ByteBuffer;  Value : out Real64) with
     Pre'Class  => Msg_Bytes_Remaining (This) >= 8,
     Post'Class => Position (This) = Position (This)'Old + 8 and
                   Length (This) = Length (This)'Old and
                   Msg_Bytes_Remaining (This) = Msg_Bytes_Remaining (This)'Old - 8;

   Runtime_Length_Error : exception;
   --  Raised by Get_String and Get_Unbounded_String when the string length
   --  read from the buffer is greater than the number of actual bytes remaining
   --  in the buffer to be fetched. This can only happen if a message coming into
   --  a buffer is somehow truncated during reception or corrupted, or if we are
   --  deserializing the buffer content incorrectly.

   procedure Get_String
     (This  : in out ByteBuffer;
      Value : out String;
      Last  : out Natural)
   with
     Pre'Class  =>
       Msg_Bytes_Remaining (This) >= 2,
       --  The string content is preceded in the buffer by a two-byte length,
       --  even when zero, so the precondition checks that there are least
       --  two bytes available for the length. (If the length is zero there
       --  will be no further string bytes to read.)
       --
       --  NB: At run-time, Runtime_Length_Error is raised if the serialized
       --  length value in the buffer indicates a length greater than the
       --  number of message bytes remaining in the buffer. This ensures we
       --  don't use a corrupted length (e.g., to read past the buffer).
     Post'Class =>
       --  Last is Value'First - 1 when the number of characters is read as
       --  zero, otherwise it is in Value'Range
       Last in Value'First - 1 .. Value'Last and
       --  we read the length, which was zero, so nothing else was read
       (if Last = Value'First - 1  then Position (This) = Position (This'Old) + 2) and
       --  we read the length, which was nonzero, and then that many characters
       (if Last /= Value'First - 1 then Position (This) = Position (This'Old) + 2 + UInt32(Last - Value'First + 1)) and
       Length (This) = Length (This)'Old;

   procedure Get_Unbounded_String
     (This  : in out ByteBuffer;
      Value : out Unbounded_String)
   with
     Pre'Class  =>
       Msg_Bytes_Remaining (This) >= 2,
       --  The string content is preceded in the buffer by a two-byte length,
       --  even when zero, so the precondition checks that there are least
       --  two bytes available for the length. (If the length is zero there
       --  will be no further string bytes to read.)
       --
       --  NB: At run-time, Runtime_Length_Error is raised if the serialized
       --  length value in the buffer indicates a length greater than the
       --  number of message bytes remaining in the buffer. This ensures we
       --  don't use a corrupted length (e.g., to read past the buffer).
     Post'Class =>
       --  we read the length, which was zero, so nothing else was read
       (if Value = Null_Unbounded_String  then Position (This) = Position (This'Old) + 2) and
       --  we read the length, which was nonzero, and then that many characters
       (if Value /= Null_Unbounded_String then Position (This) = Position (This'Old) + 2 + UInt32 (Length (Value))) and
       Length (This) = Length (This)'Old;

   procedure Put_Byte (This : in out ByteBuffer;  Value : Byte) with
     Pre'Class  => Space_Available (This) >= 1,
     Post'Class => Position (This) = Position (This)'Old + 1 and
                   Length (This) = Length (This)'Old + 1 and
                   Space_Available (This) = Space_Available (This)'Old - 1;

   procedure Put_Boolean (This : in out ByteBuffer;  Value : Boolean) with
     Pre'Class  => Space_Available (This) >= 1,
     Post'Class => Position (This) = Position (This)'Old + 1 and
                   Length (This) = Length (This)'Old + 1 and
                   Space_Available (This) = Space_Available (This)'Old - 1;

   procedure Put_Int16 (This : in out ByteBuffer;  Value : Int16) with
     Pre'Class  => Space_Available (This) >= 2,
     Post'Class => Position (This) = Position (This)'Old + 2 and
                   Length (This) = Length (This)'Old + 2 and
                   Space_Available (This) = Space_Available (This)'Old - 2;

   procedure Put_Short (This : in out ByteBuffer;  Value : Int16) renames Put_Int16;

   procedure Put_UInt16 (This : in out ByteBuffer;  Value : UInt16) with
     Pre'Class  => Space_Available (This) >= 2,
     Post'Class => Position (This) = Position (This)'Old + 2 and
                   Length (This) = Length (This)'Old + 2 and
                   Space_Available (This) = Space_Available (This)'Old - 2;

   procedure Put_UShort (This : in out ByteBuffer;  Value : UInt16) renames Put_UInt16;

   procedure Put_Int32 (This : in out ByteBuffer;  Value : Int32) with
     Pre'Class  => Space_Available (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4 and
                   Length (This) = Length (This)'Old + 4 and
                   Space_Available (This) = Space_Available (This)'Old - 4;

   procedure Put_Int (This : in out ByteBuffer;  Value : Int32) renames Put_Int32;

   procedure Put_UInt32 (This : in out ByteBuffer;  Value : UInt32) with
     Pre'Class  => Space_Available (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4 and
                   Length (This) = Length (This)'Old + 4 and
                   Space_Available (This) = Space_Available (This)'Old - 4;

   procedure Put_UInt (This : in out ByteBuffer;  Value : UInt32) renames Put_UInt32;

   procedure Put_Int64 (This : in out ByteBuffer;  Value : Int64) with
     Pre'Class  => Space_Available (This) >= 8,
     Post'Class => Position (This) = Position (This)'Old + 8 and
                   Length (This) = Length (This)'Old + 8 and
                   Space_Available (This) = Space_Available (This)'Old - 8;

   procedure Put_Long (This : in out ByteBuffer;  Value : Int64) renames Put_Int64;

   procedure Put_UInt64 (This : in out ByteBuffer;  Value : UInt64) with
     Pre'Class  => Space_Available (This) >= 8,
     Post'Class => Position (This) = Position (This)'Old + 8 and
                   Length (This) = Length (This)'Old + 8 and
                   Space_Available (This) = Space_Available (This)'Old - 8;

   procedure Put_ULong (This : in out ByteBuffer;  Value : UInt64) renames Put_UInt64;

   procedure Put_Real32 (This : in out ByteBuffer;  Value : Real32) with
     Pre'Class  => Space_Available (This) >= 4,
     Post'Class => Position (This) = Position (This)'Old + 4 and
                   Length (This) = Length (This)'Old + 4 and
                   Space_Available (This) = Space_Available (This)'Old - 4;

   procedure Put_Real64 (This : in out ByteBuffer;  Value : Real64) with
     Pre'Class  => Space_Available (This) >= 8,
     Post'Class => Position (This) = Position (This)'Old + 8 and
                   Length (This) = Length (This)'Old + 8 and
                   Space_Available (This) = Space_Available (This)'Old - 8;

   Max_String_Length : constant := UInt16'Last;
   --  Like the C++ version, we write strings lengths as a UInt16 during
   --  serialization, so we allow at most a length of UInt16'Last. The C++
   --  version truncates and prints an error msg at run-time for lengths
   --  greater than UInt16'Last. With the precondition we will get an
   --  exception, a more robust approach than losing message content
   --  silently without any functional notification of such.

   procedure Put_String (This : in out ByteBuffer;  Value : String) with
     Pre'Class  =>
       Value'Length <= Max_String_Length and then
       Space_Available (This) >= Value'Length + 2,  -- 2 bytes for the length
     Post'Class =>
       Position (This) = Position (This)'Old + Value'Length + 2 and
       Length (This) = Length (This)'Old + Value'Length + 2     and
       Space_Available (This) = Space_Available (This)'Old - Value'Length - 2;

   procedure Put_Unbounded_String (This : in out ByteBuffer; Value : Unbounded_String) with
     Pre'Class  =>
       Length (Value) <= Max_String_Length and then
       Space_Available (This) >= UInt32 (Length (Value)) + 2,  -- 2 bytes for the length
     Post'Class =>
       Position (This) = Position (This)'Old + UInt32 (Length (Value)) + 2 and
       Length (This) = Length (This)'Old + UInt32 (Length (Value)) + 2     and
       Space_Available (This) = Space_Available (This)'Old - UInt32 (Length (Value)) - 2;

   --  The following two Put_Raw_Bytes routines populate the ByteBuffer from
   --  the bytes in an array. One of the input array types is a String, the
   --  other just an array of bytes. These are useful for then rewinding
   --  and reading back out meaningful objects. The input Value is a String
   --  because that's the most convenient choice, based on client usage.
   --
   --  NB: these routines dosn't write the length into the buffer because
   --  the content of the input string is an encoded (serialized) message
   --  already. That also means that there is no two-byte length restriction.

   procedure Put_Raw_Bytes (This : in out ByteBuffer; Value : String) with
     Pre'Class  => Space_Available (This) >= Value'Length,
     Post'Class => Position (This) = Position (This)'Old + Value'Length and
                   Length (This) = Length (This)'Old + Value'Length     and
                   Space_Available (This) = Space_Available (This)'Old - Value'Length;

   type Byte_Array is array (Index range <>) of Byte
     with Component_Size => 1 * Storage_Unit;

   procedure Put_Raw_Bytes (This : in out ByteBuffer; Value : Byte_Array) with
     Pre'Class  => Space_Available (This) >= Value'Length,
     Post'Class => Position (This) = Position (This)'Old + Value'Length and
                   Length (This) = Length (This)'Old + Value'Length     and
                   Space_Available (This) = Space_Available (This)'Old - Value'Length;

   function Raw_Bytes (This : ByteBuffer) return Byte_Array;
   --  Returns the full internal byte array content

   function Raw_Bytes (This : ByteBuffer) return String;
   --  Returns the full internal byte array content as a String

   function Checksum (This : ByteBuffer;  From, To : Index) return UInt32 with
     Pre'Class =>
       From <= To                and   -- only useful ranges
       From <= This.Capacity     and   -- physically possible
       To   <= This.Capacity     and   --     "         "
       From <= This.Length       and   -- logically possible
       To   <= This.Length;            --     "        "
   --  Computes the checksum of the slice of the internal byte array From .. To.

private

   subtype Counter is UInt32 range 0 .. Maximum_Length;

   type ByteBuffer (Capacity : Index) is tagged record
      Content          : Byte_Array (1 .. Capacity) := (others => 0);
      Position         : Index   := 1;   -- reset to 1 by Rewind
      Total_Bytes_Used : Counter := 0;   -- reset to 0 by Clear
   end record;

end AVTAS.LMCP.ByteBuffers;
