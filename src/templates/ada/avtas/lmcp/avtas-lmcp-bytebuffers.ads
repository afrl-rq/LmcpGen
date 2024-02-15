with AVTAS.LMCP.Types;      use AVTAS.LMCP.Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System;                use System;
with Ada.Unchecked_Conversion;
with GNAT.Byte_Swapping;
with Interfaces;

package AVTAS.LMCP.ByteBuffers with
   SPARK_Mode
is

   pragma Assertion_Policy (Pre => Check);
   --  The preconditions are required unless the clients are proved too. This
   --  pragma overrides the effect of the -gnata switch.

   pragma Unevaluated_Use_Of_Old (Allow);

   Maximum_Length : constant := UInt32'Last - 8;
   --  The largest value that we insert/remove (that is not an array) is eight
   --  bytes wide, therefore the max length is 8 less than 'Last in order to
   --  avoid overflow.

   type Index is new Interfaces.Integer_64 range 0 .. Interfaces.Integer_64 (Maximum_Length);

   subtype NonZero_Index is Index range 1 .. Index'Last;

   type ByteBuffer (Capacity : NonZero_Index) is private with
     Default_Initial_Condition =>
       Position (ByteBuffer) = 0 and then
       High_Water_Mark (ByteBuffer) = 0 and then
       Remaining (ByteBuffer) = Capacity;

   function Remaining (This : ByteBuffer) return Index;
   --  Returns the number of unused bytes remaining available in This

   function Position (This : ByteBuffer) return Index;
   --  Returns the next place within This buffer for reading or writing

   function High_Water_Mark (This : ByteBuffer) return Index;
   --  Returns the number of data bytes currently written into This buffer.
   --  This value is never decremented by a Get_* routine, and is only set
   --  back to zero by a call to Reset.

   procedure Rewind (This : in out ByteBuffer) with
     Post => Position (This) = 0                                 and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Raw_Bytes (This) = Raw_Bytes (This)'Old;
   --  Rewinding the buffer position allows reading of existing content from
   --  the beginning, presumably after writing values into it (via the Put_*
   --  routines).

   procedure Reset (This : in out ByteBuffer) with
     Post => Position (This) = 0 and
             High_Water_Mark (This) = 0;

   Byte_Size     : constant := 1;  -- bytes
   Boolean_Size  : constant := 1;  -- bytes
   Int16_Size    : constant := 2;  -- bytes
   UInt16_Size   : constant := 2;  -- bytes
   Int32_Size    : constant := 4;  -- bytes
   UInt32_Size   : constant := 4;  -- bytes
   Int64_Size    : constant := 8;  -- bytes
   UInt64_Size   : constant := 8;  -- bytes
   Real32_Size   : constant := 4;  -- bytes
   Real64_Size   : constant := 8;  -- bytes
   Checksum_Size : constant := 4;  -- bytes

   Max_String_Length : constant := UInt16'Last; -- ie, 65_535
   --  Like the C++ version, this is as large as a serialized (stored) string
   --  can be, because we write each string's length into the buffer as a
   --  UInt16 during serialization.

   Stored_Length_Size : constant := UInt16_Size;
   --  The size in bytes for the lengths of strings stored in buffers. Every
   --  string in a buffer is preceeded by the length of that string, even when
   --  the length is zero.

   procedure Get_Byte (This : in out ByteBuffer;  Value : out Byte) with
     Pre  => Position (This) <= High_Water_Mark (This) - Byte_Size,
     Post => Position (This) = Position (This)'Old + Byte_Size   and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) + Byte_Size = Remaining (This)'Old and then
             Raw_Bytes (This) (Position (This)'Old) = Value      and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_Boolean (This : in out ByteBuffer;  Value : out Boolean) with
     Pre  => Position (This) <= High_Water_Mark (This) - Boolean_Size,
     Post => Position (This) = Position (This)'Old + Boolean_Size   and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old    and then
             Remaining (This) + Boolean_Size = Remaining (This)'Old and then
             (Raw_Bytes (This) (Position (This)'Old) /= 0) = Value  and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_Int16 (This : in out ByteBuffer;  Value : out Int16) with
     Pre  => Position (This) <= High_Water_Mark (This) - Int16_Size,
     Post => Position (This) = Position (This)'Old + Int16_Size   and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old  and then
             Remaining (This) + Int16_Size = Remaining (This)'Old and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Two_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_UInt16 (This : in out ByteBuffer;  Value : out UInt16) with
     Pre  => Position (This) <= High_Water_Mark (This) - UInt16_Size,
     Post => Position (This) = Position (This)'Old + UInt16_Size   and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old   and then
             Remaining (This) + UInt16_Size = Remaining (This)'Old and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Two_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_Int32 (This : in out ByteBuffer;  Value : out Int32) with
     Pre  => Position (This) <= High_Water_Mark (This) - Int32_Size,
     Post => Position (This) = Position (This)'Old + Int32_Size   and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old  and then
             Remaining (This) + Int32_Size = Remaining (This)'Old and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Four_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_UInt32
     (This  : ByteBuffer;
      Value : out UInt32;
      First : Index)
   with
     Pre  => First <= This.Capacity - (UInt32_Size - 1) and then
             First + UInt32_Size - 1 <= High_Water_Mark (This) - 1,
     Post => Position (This) = Position (This)'Old               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) = Remaining (This)'Old             and then
             Raw_Bytes (This) = Raw_Bytes (This)'Old             and then
             Raw_Bytes (This) (First .. First + UInt32_Size - 1) = As_Four_Bytes (Value);
   --  Gets the four bytes comprising a UInt32 value from This buffer, starting
   --  at absolute index First (rather than from This.Position)

   procedure Get_UInt32 (This : in out ByteBuffer;  Value : out UInt32) with
     Pre  => Position (This) <= High_Water_Mark (This) - UInt32_Size,
     Post => Position (This) = Position (This)'Old + UInt32_Size   and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old    and then
             Remaining (This) + UInt32_Size = Remaining (This)'Old and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Four_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_Int64  (This : in out ByteBuffer;  Value : out Int64) with
     Pre  => Position (This) <= High_Water_Mark (This) - Int64_Size,
     Post => Position (This) = Position (This)'Old + Int64_Size   and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old   and then
             Remaining (This) + Int64_Size = Remaining (This)'Old and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Eight_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_UInt64 (This : in out ByteBuffer;  Value : out UInt64) with
     Pre  => Position (This) <= High_Water_Mark (This) - UInt64_Size,
     Post => Position (This) = Position (This)'Old + UInt64_Size   and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old   and then
             Remaining (This) + UInt64_Size = Remaining (This)'Old and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Eight_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_Real32 (This : in out ByteBuffer;  Value : out Real32) with
     Pre  => Position (This) <= High_Water_Mark (This) - Real32_Size,
     Post => Position (This) = Position (This)'Old + Real32_Size   and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old    and then
             Remaining (This) + Real32_Size = Remaining (This)'Old and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Four_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_Real64 (This : in out ByteBuffer;  Value : out Real64) with
     Pre  => Position (This) <= High_Water_Mark (This) - Real64_Size,
     Post => Position (This) = Position (This)'Old + Real64_Size   and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old    and then
             Remaining (This) + Real64_Size = Remaining (This)'Old and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Eight_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_String
     (This          : in out ByteBuffer;
      Value         : in out String;
      Last          : out Integer;
      Stored_Length : out UInt32)
      --  The length indicated by the message data in the buffer, not
      --  necessarily the length of the result.
   with
     Pre  =>
       Value'First  in Positive               and then
       Value'Last   in Positive               and then
       Value'Length in 1 .. Max_String_Length and then
       Position (This) <= High_Water_Mark (This) - Stored_Length_Size,
       --  The string content is preceded in the buffer by a two-byte length,
       --  even when the string length is zero (ie when the string is empty).
     Post =>
       High_Water_Mark (This) = High_Water_Mark (This)'Old and then
       Stored_Length <= Max_String_Length                  and then
       Raw_Bytes (This) (Position (This)'Old .. Position (This)'Old + 1) = As_Two_Bytes (UInt16 (Stored_Length)) and then
       (if Index (Stored_Length) > Remaining (This)'Old - Stored_Length_Size then
           --  This is a corrupted buffer: there are too few remaining bytes
           --  available in the buffer to be assigned to the String Value.
           Last = -1 and
           Position (This) = Position (This)'Old + Stored_Length_Size
        elsif Stored_Length > Value'Length then
           --  Presumably client error; the buffer is not necessarily corrupted.
           Last = -1 and
           Position (This) = Position (This)'Old + Stored_Length_Size
        elsif High_Water_Mark (This) + Index (Stored_length) > This.Capacity then
           Last = -1 and
           Position (This) = Position (This)'Old + Stored_Length_Size
        elsif Position (This)'Old + Stored_Length_Size + Index (Stored_Length) > High_Water_Mark (This)'Old then
           Last = -1 and
           Position (This) = Position (This)'Old + Stored_Length_Size
        elsif Stored_Length = 0 then
           --  This is a normal case, specifically for an empty string.
           Last = Value'First - 1 and then
           Position (This) = Position (This)'Old + Stored_Length_Size
        else
           --  This is a normal case for a non-empty string in the buffer. If
           --  the stored length is <= Value'Length we only read Stored_Length
           --  number of chars from the buffer, potentially leaving some of
           --  Value unassigned (hence the need for Last).
           Last = Value'First + (Positive (Stored_Length) - 1)                                and then
           Position (This) = Position (This)'Old + Stored_Length_Size + Index (Stored_Length) and then
           Equal (Raw_Bytes (This) (Position (This)'Old + Stored_Length_Size .. Position (This) - 1),
                  Value (Value'First .. Last)));

   procedure Get_Unbounded_String
     (This          : in out ByteBuffer;
      Value         : out Unbounded_String;
      Stored_Length : out UInt32)
   with
     Pre  => Position (This) <= High_Water_Mark (This) - Stored_Length_Size,
             --  The string content is preceded in the buffer by a two-byte length,
             --  even when the string length is zero (ie when the string is empty).
     Post => High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Stored_Length <= Max_String_Length and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This)'Old + 1) = As_Two_Bytes (UInt16 (Stored_Length)) and then
             (if Index (Stored_Length) > Remaining (This)'Old - Stored_Length_Size or else
                 Position (This)'Old + Stored_Length_Size + Index (Stored_length) > High_Water_Mark (This)
              then
                 To_String (Value) = "Corrupted buffer" and then
                 Position (This) = Position (This)'Old + Stored_Length_Size
              elsif Stored_Length = 0 then
                 Position (This) = Position (This)'Old + Stored_Length_Size
              else
                 Position (This) = Position (This)'Old + Stored_Length_Size + Index (Stored_Length) and then
                 Equal (Raw_Bytes (This) (Position (This)'Old + Stored_Length_Size .. Position (This) - 1),
                        To_String (Value)));

   procedure Put_Byte (This : in out ByteBuffer;  Value : Byte) with
     Pre  => Remaining (This) >= Byte_Size and then
             High_Water_Mark (This) <= This.Capacity - Byte_Size,
     Post => Position (This) = Position (This)'Old + Byte_Size                 and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old              and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + Byte_Size  and then
             --  We have the expected bytes remaining available
             Remaining (This) = Remaining (This)'Old - Byte_Size               and then
             --  The new value in the model is as expected ...
             Raw_Bytes (This) (Position (This)'Old) = Value                    and then
             --  Nothing else has changed
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   procedure Put_Boolean (This : in out ByteBuffer;  Value : Boolean) with
     Pre  => Remaining (This) >= Boolean_Size and then
             High_Water_Mark (This) <= This.Capacity - Boolean_Size,
     Post => Position (This) = Position (This)'Old + Boolean_Size                 and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old                 and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + Boolean_Size  and then
             --  We have the expected bytes remaining available
             Remaining (This) = Remaining (This)'Old - Boolean_Size               and then
             --  The new value in the model is as expected ...
             Raw_Bytes (This) (Position (This)'Old) = Boolean'Pos (Value)         and then
             --  Nothing else has changed
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   procedure Put_Int16 (This : in out ByteBuffer;  Value : Int16) with
     Pre  => Remaining (This) >= Int16_Size and then
             High_Water_Mark (This) <= This.Capacity - Int16_Size,
     Post => Position (This) = Position (This)'Old + Int16_Size                 and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old               and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + Int16_Size  and then
             --  We have the expected bytes remaining available
             Remaining (This) = Remaining (This)'Old - Int16_Size               and then
             --  The new value in the model is as expected ...
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Two_Bytes (Value) and then
             --  Nothing else has changed
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   procedure Put_UInt16 (This : in out ByteBuffer;  Value : UInt16) with
     Pre  => Remaining (This) >= UInt16_Size and then
             High_Water_Mark (This) <= This.Capacity - UInt16_Size,
     Post => Position (This) = Position (This)'Old + UInt16_Size                 and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old                and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + UInt16_Size  and then
             --  We have the expected bytes remaining available
             Remaining (This) = Remaining (This)'Old - UInt16_Size               and then
             --  The new value in the model is as expected ...
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Two_Bytes (Value) and then
             --  Nothing else has changed
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   procedure Put_Int32 (This : in out ByteBuffer;  Value : Int32) with
     Pre  => Remaining (This) >= Int32_Size and then
             High_Water_Mark (This) <= This.Capacity - Int32_Size,
     Post => Position (This) = Position (This)'Old + Int32_Size                 and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old               and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + Int32_Size  and then
             --  We have the expected bytes remaining available
             Remaining (This) = Remaining (This)'Old - Int32_Size               and then
             --  The new value in the model is as expected ...
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Four_Bytes (Value) and then
             --  Nothing else has changed
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   procedure Put_UInt32 (This : in out ByteBuffer;  Value : UInt32) with
     Pre  => Remaining (This) >= UInt32_Size and then
             High_Water_Mark (This) <= This.Capacity - UInt32_Size,
     Post => Position (This) = Position (This)'Old + UInt32_Size                 and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old                and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + UInt32_Size  and then
             --  We have the expected bytes remaining available
             Remaining (This) = Remaining (This)'Old - UInt32_Size               and then
             --  The new value in the model is as expected ...
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Four_Bytes (Value) and then
             --  Nothing else has changed
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   procedure Put_Int64 (This : in out ByteBuffer;  Value : Int64) with
     Pre  => Remaining (This) >= Int64_Size and then
             High_Water_Mark (This) <= This.Capacity - Int64_Size,
     Post => Position (This) = Position (This)'Old + Int64_Size                 and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old               and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + Int64_Size  and then
             --  We have the expected bytes remaining available
             Remaining (This) = Remaining (This)'Old - Int64_Size               and then
             --  The new value in the model is as expected ...
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Eight_Bytes (Value) and then
             --  Nothing else has changed
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   procedure Put_UInt64 (This : in out ByteBuffer;  Value : UInt64) with
     Pre  => Remaining (This) >= UInt64_Size and then
             High_Water_Mark (This) <= This.Capacity - UInt64_Size,
     Post => Position (This) = Position (This)'Old + UInt64_Size                 and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old                and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + UInt64_Size  and then
             --  We have the expected bytes remaining available
             Remaining (This) = Remaining (This)'Old - UInt64_Size               and then
             --  The new value in the model is as expected ...
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Eight_Bytes (Value) and then
             --  Nothing else has changed
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   procedure Put_Real32 (This : in out ByteBuffer;  Value : Real32) with
     Pre  => Remaining (This) >= Real32_Size and then
             High_Water_Mark (This) <= This.Capacity - Real32_Size,
     Post => Position (This) = Position (This)'Old + Real32_Size                 and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old                and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + Real32_Size  and then
             --  We have the expected bytes remaining available
             Remaining (This) = Remaining (This)'Old - Real32_Size               and then
             --  The new value in the model is as expected ...
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Four_Bytes (Value) and then
             --  Nothing else has changed
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   procedure Put_Real64 (This : in out ByteBuffer;  Value : Real64) with
     Pre  => Remaining (This) >= Real64_Size and then
             High_Water_Mark (This) <= This.Capacity - Real64_Size,
     Post => Position (This) = Position (This)'Old + Real64_Size                 and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old                and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + Real64_Size  and then
             --  We have the expected bytes remaining available
             Remaining (This) = Remaining (This)'Old - Real64_Size               and then
             --  The new value in the model is as expected ...
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Eight_Bytes (Value) and then
             --  Nothing else has changed
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   procedure Put_String (This : in out ByteBuffer;  Value : String) with
     Pre  => Value'First = 1                                       and then
             Value'Length <= Max_String_Length                     and then
             Remaining (This) >= Value'Length + Stored_Length_Size and then
             High_Water_Mark (This) <= This.Capacity - Value'Length - Stored_Length_Size,
     Post => Position (This) = Position (This)'Old + Value'Length + Stored_Length_Size                 and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old                                      and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + Value'Length + Stored_Length_Size  and then
             Remaining (This) = Remaining (This)'Old - Value'Length - Stored_Length_Size               and then
             New_Content_Equal (This, Position (This)'Old + Stored_Length_Size, Value)                 and then
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   procedure Put_Unbounded_String (This : in out ByteBuffer; Value : Unbounded_String) with
     Pre  => Length (Value) <= Max_String_Length and then
             Remaining (This) >= Index (Length (Value)) + Stored_Length_Size and then
             High_Water_Mark (This) <= This.Capacity - Index (Length (Value)) - Stored_Length_Size,
     Post => Position (This) = Position (This)'Old + Index (Length (Value)) + Stored_Length_Size                and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old                                               and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + Index (Length (Value)) + Stored_Length_Size and then
             Remaining (This) = Remaining (This)'Old - Index (Length (Value)) - Stored_Length_Size              and then
             New_Content_Equal (This, Position (This)'Old + Stored_Length_Size, To_String (Value))              and then
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   --  The following two Put_Raw_Bytes routines populate the ByteBuffer from
   --  the bytes in an array. One of the input array types is a String, the
   --  other just an array of bytes. These are useful for then rewinding
   --  and reading back out meaningful objects. The type String is supported
   --  because that's the most convenient choice, based on client usage.
   --
   --  NB: these routines don't write the length into the buffer because
   --  the content of the input string is an encoded (serialized) message
   --  already. That also means that there is no two-byte length restriction.

   procedure Put_Raw_Bytes (This : in out ByteBuffer; Value : String) with
     Pre  => Remaining (This) >= Value'Length  and then
             High_Water_Mark (This) + Value'Length <= This.Capacity,
     Post => Position (This) = Position (This)'Old + Value'Length                and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old                and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + Value'Length and then
             Remaining (This) = Remaining (This)'Old - Value'Length              and then
             New_Content_Equal (This, Position (This)'Old, Value)                and then
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   type Byte_Array is array (Index range <>) of Byte with
     Component_Size => Byte'Size;

   procedure Put_Raw_Bytes (This : in out ByteBuffer; Value : Byte_Array) with
     Pre  => Remaining (This) >= Value'Length and then
             High_Water_Mark (This) + Value'Length <= This.Capacity,
     Post => Position (This) = Position (This)'Old + Value'Length                and then
             --  High_Water_Mark is incremented iff new Position would exceed it
             High_Water_Mark (This) >= High_Water_Mark (This)'Old                and then
             High_Water_Mark (This) <= High_Water_Mark (This)'Old + Value'Length and then
             Remaining (This) = Remaining (This)'Old - Value'Length              and then
             New_Content_Equal (This, Position (This)'Old, Value)                and then
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   function Raw_Bytes (This : ByteBuffer) return Byte_Array;
   --  Returns the entire internal byte array content, up to but not including
   --  High_Water_Mark (This)

   function Raw_Bytes_As_String (This : ByteBuffer) return String with
     Pre  => High_Water_Mark (This) <= Index (Positive'Last),
     Post => Raw_Bytes_As_String'Result'First = 1 and then
             Raw_Bytes_As_String'Result'Length = High_Water_Mark (This);
   --  Returns the entire internal byte array content, as a String

   function Checksum (This : ByteBuffer;  Last : Index) return UInt32 with
     Pre => Last <= High_Water_Mark (This) - 1;
   --  Computes the checksum of the slice of the internal byte array from the
   --  first byte up to Last (inclusive). 

   subtype Two_Bytes   is Byte_Array (0 .. 1) with Object_Size => 16;
   subtype Four_Bytes  is Byte_Array (0 .. 3) with Object_Size => 32;
   subtype Eight_Bytes is Byte_Array (0 .. 7) with Object_Size => 64;

   function As_Two_Bytes (Value : Int16)  return Two_Bytes;
   --  Returns Value, with bytes swapped if necessary, as Two_Bytes
   function As_Two_Bytes (Value : UInt16) return Two_Bytes;
   --  Returns Value, with bytes swapped if necessary, as Two_Bytes

   function As_Four_Bytes (Value : Real32) return Four_Bytes;
   --  Returns Value, with bytes swapped if necessary, as Four_Bytes
   function As_Four_Bytes (Value : Int32)  return Four_Bytes;
   --  Returns Value, with bytes swapped if necessary, as Four_Bytes
   function As_Four_Bytes (Value : UInt32) return Four_Bytes;
   --  Returns Value, with bytes swapped if necessary, as Four_Bytes

   function As_Eight_Bytes (Value : Real64) return Eight_Bytes;
   --  Returns Value, with bytes swapped if necessary, as Eight_Bytes
   function As_Eight_Bytes (Value : Int64)  return Eight_Bytes;
   --  Returns Value, with bytes swapped if necessary, as Eight_Bytes
   function As_Eight_Bytes (Value : UInt64) return Eight_Bytes;
   --  Returns Value, with bytes swapped if necessary, as Eight_Bytes

   ----------------------
   --  Ghost routines  --
   ----------------------

   function Prior_Content_Unchanged (New_Value, Old_Value : ByteBuffer) return Boolean with
     Ghost,
     Pre => High_Water_Mark (New_Value) >= High_Water_Mark (Old_Value);
   --  Returns whether the content of Old_Value was unchanged in New_Value

   function New_Content_Equal
     (This      : ByteBuffer;
      Start     : Index;
      Comparand : String)
   return Boolean
   with
     Ghost,
     Pre => Start <= High_Water_Mark (This) - Comparand'Length;
   --  Returns whether the slice of This, starting at Start, equals the
   --  Comparand of type String

   function New_Content_Equal
     (This      : ByteBuffer;
      Start     : Index;
      Comparand : Byte_Array)
   return Boolean
   with
     Ghost,
     Pre => Start <= High_Water_Mark (This) - Comparand'Length;
   --  Returns whether the slice of This, starting at Start, equals the
   --  Comparand of type Byte_Array

   function Equal (Left : Byte_Array; Right : String) return Boolean with
     Ghost;
   --  Returns whether the individual bytes of the Left Byte_Array, when
   --  considered as Character values, equal the individual characters of
   --  the Right String

private

   type Byte_Data is array (Index range <>) of Byte with
     Component_Size => Byte'Size,
     Relaxed_Initialization;

   type ByteBuffer (Capacity : NonZero_Index) is record
      Content           : Byte_Data (0 .. Capacity);
      Position          : Index := 0;
      Highest_Write_Pos : Index := 0;
   end record with
      Type_Invariant =>
        Highest_Write_Pos <= Capacity and then
        Position <= Highest_Write_Pos and then
        (for all K in 0 .. Highest_Write_Pos - 1 => Content (K)'Initialized);
   pragma Annotate (GNATProve,
                    False_Positive,
                    "type ""ByteBuffer"" is not fully initialized",
                    "bogus interaction between DIC and Relaxed_Init");

   ---------------
   -- Raw_Bytes --
   ---------------

   function Raw_Bytes (This : ByteBuffer) return Byte_Array is
      (Byte_Array (This.Content (0 .. This.Highest_Write_Pos - 1)));

   ---------------
   -- Remaining --
   ---------------

   function Remaining (This : ByteBuffer) return Index is
      (This.Capacity - This.Position);

   --------------
   -- Position --
   --------------

   function Position (This : ByteBuffer) return Index is
     (This.Position);

   ---------------------
   -- High_Water_Mark --
   ---------------------

   function High_Water_Mark (This : ByteBuffer) return Index is
     (This.Highest_Write_Pos);

   --------------------------------------------------------------------
   --  internal routines used in the following expression functions  --
   --------------------------------------------------------------------

   function To_Eight_Bytes is new Ada.Unchecked_Conversion
     (Source => Real64, Target => Eight_Bytes);

   function To_Eight_Bytes is new Ada.Unchecked_Conversion
     (Source => Int64, Target => Eight_Bytes);

   function To_Eight_Bytes is new Ada.Unchecked_Conversion
     (Source => UInt64, Target => Eight_Bytes);

   function To_Four_Bytes is new Ada.Unchecked_Conversion
     (Source => Real32, Target => Four_Bytes);

   function To_Four_Bytes is new Ada.Unchecked_Conversion
     (Source => Int32, Target => Four_Bytes);

   function To_Four_Bytes is new Ada.Unchecked_Conversion
     (Source => UInt32, Target => Four_Bytes);

   function To_Two_Bytes is new Ada.Unchecked_Conversion
     (Source => Int16, Target => Two_Bytes);

   function To_Two_Bytes is new Ada.Unchecked_Conversion
     (Source => UInt16, Target => Two_Bytes);

   function To_Int16 is new Ada.Unchecked_Conversion
     (Source => Two_Bytes, Target => Int16);

   function To_UInt16 is new Ada.Unchecked_Conversion
     (Source => Two_Bytes, Target => UInt16);

   function To_Int32 is new Ada.Unchecked_Conversion
     (Source => Four_Bytes, Target => Int32);

   function To_UInt32 is new Ada.Unchecked_Conversion
     (Source => Four_Bytes, Target => UInt32);

   function To_Int64 is new Ada.Unchecked_Conversion
     (Source => Eight_Bytes, Target => Int64);

   function To_UInt64 is new Ada.Unchecked_Conversion
     (Source => Eight_Bytes, Target => UInt64);

   function Swapped is new GNAT.Byte_Swapping.Swapped8 (Real64);

   function Swapped is new GNAT.Byte_Swapping.Swapped8 (Int64);

   function Swapped is new GNAT.Byte_Swapping.Swapped8 (UInt64);

   function Swapped is new GNAT.Byte_Swapping.Swapped4 (Real32);

   function Swapped is new GNAT.Byte_Swapping.Swapped4 (Int32);

   function Swapped is new GNAT.Byte_Swapping.Swapped4 (UInt32);

   function Swapped is new GNAT.Byte_Swapping.Swapped2 (Int16);

   function Swapped is new GNAT.Byte_Swapping.Swapped2 (UInt16);

   --------------------
   -- As_Eight_Bytes --
   --------------------

   pragma Warnings (Off, "Unreachable branch");
   function As_Eight_Bytes (Value : Real64) return Eight_Bytes is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        To_Eight_Bytes (Swapped (Value))
      else
        To_Eight_Bytes (Value));
   pragma Warnings (On, "Unreachable branch");

   --------------------
   -- As_Eight_Bytes --
   --------------------

   pragma Warnings (Off, "Unreachable branch");
   function As_Eight_Bytes (Value : Int64) return Eight_Bytes is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        To_Eight_Bytes (Swapped (Value))
      else
        To_Eight_Bytes (Value));
   pragma Warnings (On, "Unreachable branch");

   --------------------
   -- As_Eight_Bytes --
   --------------------

   pragma Warnings (Off, "Unreachable branch");
   function As_Eight_Bytes (Value : UInt64) return Eight_Bytes is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        To_Eight_Bytes (Swapped (Value))
      else
        To_Eight_Bytes (Value));
   pragma Warnings (On, "Unreachable branch");

   -------------------
   -- As_Four_Bytes --
   -------------------

   pragma Warnings (Off, "Unreachable branch");
   function As_Four_Bytes (Value : Real32) return Four_Bytes is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        To_Four_Bytes (Swapped (Value))
      else
        To_Four_Bytes (Value));
   pragma Warnings (On, "Unreachable branch");

   -------------------
   -- As_Four_Bytes --
   -------------------

   pragma Warnings (Off, "Unreachable branch");
   function As_Four_Bytes (Value : Int32) return Four_Bytes is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        To_Four_Bytes (Swapped (Value))
      else
        To_Four_Bytes (Value));
   pragma Warnings (On, "Unreachable branch");

   -------------------
   -- As_Four_Bytes --
   -------------------

   pragma Warnings (Off, "Unreachable branch");
   function As_Four_Bytes (Value : UInt32) return Four_Bytes is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        To_Four_Bytes (Swapped (Value))
      else
        To_Four_Bytes (Value));
   pragma Warnings (On, "Unreachable branch");


   ------------------
   -- As_Two_Bytes --
   ------------------

   --  pragma Warnings (Off, "Unreachable branch");
   function As_Two_Bytes (Value : Int16) return Two_Bytes is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        To_Two_Bytes (Swapped (Value))
      else
        To_Two_Bytes (Value));
   --  pragma Warnings (On, "Unreachable branch");

   ------------------
   -- As_Two_Bytes --
   ------------------

   pragma Warnings (Off, "Unreachable branch");
   function As_Two_Bytes (Value : UInt16) return Two_Bytes is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        To_Two_Bytes (Swapped (Value))
      else
        To_Two_Bytes (Value));
   pragma Warnings (On, "Unreachable branch");

   --------------
   -- As_Int16 --
   --------------

   pragma Warnings (Off, "Unreachable branch");
   function As_Int16 (Value : Two_Bytes) return Int16 is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        Swapped (To_Int16 (Value))
      else
        To_Int16 (Value));
   pragma Warnings (On, "Unreachable branch");

   ---------------
   -- As_UInt16 --
   ---------------

   pragma Warnings (Off, "Unreachable branch");
   function As_UInt16 (Value : Two_Bytes) return UInt16 is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        Swapped (To_UInt16 (Value))
      else
        To_UInt16 (Value));
   pragma Warnings (On, "Unreachable branch");

   --------------
   -- As_Int32 --
   --------------

   pragma Warnings (Off, "Unreachable branch");
   function As_Int32 (Value : Four_Bytes) return Int32 is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        Swapped (To_Int32 (Value))
      else
        To_Int32 (Value));
   pragma Warnings (On, "Unreachable branch");

   ---------------
   -- As_UInt32 --
   ---------------

   pragma Warnings (Off, "Unreachable branch");
   function As_UInt32 (Value : Four_Bytes) return UInt32 is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        Swapped (To_UInt32 (Value))
      else
        To_UInt32 (Value));
   pragma Warnings (On, "Unreachable branch");

   --------------
   -- As_Int64 --
   --------------

   pragma Warnings (Off, "Unreachable branch");
   function As_Int64 (Value : Eight_Bytes) return Int64 is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        Swapped (To_Int64 (Value))
      else
        To_Int64 (Value));
   pragma Warnings (On, "Unreachable branch");

   ---------------
   -- As_UInt64 --
   ---------------

   pragma Warnings (Off, "Unreachable branch");
   function As_UInt64 (Value : Eight_Bytes) return UInt64 is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        Swapped (To_UInt64 (Value))
      else
        To_UInt64 (Value));
   pragma Warnings (On, "Unreachable branch");

   -----------------------------
   -- Prior_Content_Unchanged --
   -----------------------------

   function Prior_Content_Unchanged (New_Value, Old_Value : ByteBuffer) return Boolean is
     (if Old_Value.Position > 0 then  -- not empty, otherwise Unchanged'Result is trivially true
       (New_Value.Content (0 .. Old_Value.Position - 1) = Old_Value.Content (0 .. Old_Value.Position - 1)));

   -----------------------
   -- New_Content_Equal --
   -----------------------

   function New_Content_Equal
     (This      : ByteBuffer;
      Start     : Index;
      Comparand : String)
   return
      Boolean
   is
     (if Comparand'Length > 0 then
       (for all K in 1 .. Comparand'Length =>
           This.Content (Start + Index (K) - 1) = Character'Pos (Comparand (Comparand'First - 1 + K))));

   -----------------------
   -- New_Content_Equal --
   -----------------------

   function New_Content_Equal
     (This      : ByteBuffer;
      Start     : Index;
      Comparand : Byte_Array)
   return
      Boolean
   is
     (if Comparand'Length > 0 then
         Byte_Array (This.Content (Start .. Start + Comparand'Length - 1)) = Comparand);

   -----------
   -- Equal --
   -----------

   function Equal (Left : Byte_Array; Right : String) return Boolean is
     (Left'Length = Right'Length and then
      (for all K in Integer range 0 .. Left'Length - 1 =>
          Character'Val (Left (Left'First + Index (K))) = Right (Right'First + K)));

end AVTAS.LMCP.ByteBuffers;
