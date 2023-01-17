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

   type ByteBuffer (Capacity : NonZero_Index) is tagged private with
     Default_Initial_Condition =>
       Position (ByteBuffer) = 0 and
       Remaining (ByteBuffer) = Capacity;

   function Remaining (This : ByteBuffer'Class) return Index;
   --  Returns the number of unused bytes remaining available in This

   function Position (This : ByteBuffer'Class) return Index;
   --  Returns the next place within This buffer for reading or writing

   function High_Water_Mark (This : ByteBuffer'Class) return Index;
   --  Returns the number of data bytes currently written into This buffer.
   --  This value is never decremented by a Get_* routine, and is only set
   --  back to 0 by a call to Reset.

   procedure Rewind (This : in out ByteBuffer'Class) with
     Post => Position (This) = 0                                 and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Raw_Bytes (This) = Raw_Bytes (This)'Old;
   --  Rewinding the buffer position allows reading of existing content from
   --  the beginning, presumably after writing values into it (via the Put_*
   --  routines).

   procedure Reset (This : in out ByteBuffer'Class) with
     Post => Position (This) = 0 and
             High_Water_Mark (This) = 0;

   procedure Get_Byte (This : in out ByteBuffer'Class;  Value : out Byte) with
     Pre  => Remaining (This) >= 1 and then
             Position (This) <= High_Water_Mark (This) - 1,
     Post => Position (This) = Position (This)'Old + 1           and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) + 1 = Remaining (This)'Old         and then
             Raw_Bytes (This) (Position (This)'Old) = Value      and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_Boolean (This : in out ByteBuffer'Class;  Value : out Boolean) with
     Pre  => Remaining (This) >= 1 and then
             Position (This) <= High_Water_Mark (This) - 1,
     Post => Position (This) = Position (This)'Old + 1             and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old   and then
             Remaining (This) + 1 = Remaining (This)'Old           and then
             (Raw_Bytes (This) (Position (This)'Old) /= 0) = Value and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_Int16 (This : in out ByteBuffer'Class;  Value : out Int16) with
     Pre  => Remaining (This) >= 2 and then
             Position (This) <= High_Water_Mark (This) - 2,
     Post => Position (This) = Position (This)'Old + 2           and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) + 2 = Remaining (This)'Old         and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Two_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_UInt16 (This : in out ByteBuffer'Class;  Value : out UInt16) with
     Pre  => Remaining (This) >= 2 and then
             Position (This) <= High_Water_Mark (This) - 2,
     Post => Position (This) = Position (This)'Old + 2           and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) + 2 = Remaining (This)'Old         and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Two_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_Int32 (This : in out ByteBuffer'Class;  Value : out Int32) with
     Pre  => Remaining (This) >= 4 and then
             Position (This) <= High_Water_Mark (This) - 4,
     Post => Position (This) = Position (This)'Old + 4           and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) + 4 = Remaining (This)'Old         and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Four_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_UInt32
     (This  : ByteBuffer'Class;
      Value : out UInt32;
      First : Index)
   with
     Pre  => First <= This.Capacity - 3 and then
             First + 3 <= High_Water_Mark (This) - 1,
     Post => Position (This) = Position (This)'Old               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) = Remaining (This)'Old             and then
             Raw_Bytes (This) = Raw_Bytes (This)'Old             and then
             Raw_Bytes (This) (First .. First + 3) = As_Four_Bytes (Value);
   --  Gets the four bytes comprising a UInt32 value from This buffer, starting
   --  at absolute index First (rather than from This.Position)

   procedure Get_UInt32 (This : in out ByteBuffer'Class;  Value : out UInt32) with
     Pre  => Remaining (This) >= 4 and then
             Position (This) <= High_Water_Mark (This) - 4,
     Post => Position (This) = Position (This)'Old + 4           and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) + 4 = Remaining (This)'Old         and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Four_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_Int64  (This : in out ByteBuffer'Class;  Value : out Int64) with
     Pre  => Remaining (This) >= 8 and then
             Position (This) <= High_Water_Mark (This) - 8,
     Post => Position (This) = Position (This)'Old + 8           and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) + 8 = Remaining (This)'Old         and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Eight_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_UInt64 (This : in out ByteBuffer'Class;  Value : out UInt64) with
     Pre  => Remaining (This) >= 8 and then
             Position (This) <= High_Water_Mark (This) - 8,
     Post => Position (This) = Position (This)'Old + 8           and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) + 8 = Remaining (This)'Old         and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Eight_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_Real32 (This : in out ByteBuffer'Class;  Value : out Real32) with
     Pre  => Remaining (This) >= 4 and then
             Position (This) <= High_Water_Mark (This) - 4,
     Post => Position (This) = Position (This)'Old + 4           and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) + 4 = Remaining (This)'Old         and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Four_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Get_Real64 (This : in out ByteBuffer'Class;  Value : out Real64) with
     Pre  => Remaining (This) >= 8 and then
             Position (This) <= High_Water_Mark (This) - 8,
     Post => Position (This) = Position (This)'Old + 8           and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) + 8 = Remaining (This)'Old         and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Eight_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   Max_String_Length : constant := 65_535;
   --  Like the C++ version, we write strings' lengths as a UInt16 during
   --  serialization.

   procedure Get_String
     (This          : in out ByteBuffer'Class;
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
       Remaining (This) >= 2                  and then
       Position (This) <= High_Water_Mark (This) - 2,
       --  The string content is preceded in the buffer by a two-byte length,
       --  even when the string length is zero (ie when the string is empty).
     Post =>
       High_Water_Mark (This) = High_Water_Mark (This)'Old and then
       Stored_Length <= Max_String_Length                  and then
       Raw_Bytes (This) (Position (This)'Old .. Position (This)'Old + 1) = As_Two_Bytes (UInt16 (Stored_Length)) and then
       (if Index (Stored_Length) > Remaining (This)'Old - 2 then
           --  This is a corrupted buffer: there are too few remaining bytes
           --  available in the buffer to be assigned to the String Value.
           Last = -1 and
           Position (This) = Position (This)'Old + 2
        elsif Stored_Length > Value'Length then
           --  Presumably client error; the buffer is not necessarily corrupted.
           Last = -1 and
           Position (This) = Position (This)'Old + 2
        elsif High_Water_Mark (This) + Index (Stored_length) > This.Capacity then
           Last = -1 and
           Position (This) = Position (This)'Old + 2
        elsif Position (This)'Old + 2 + Index (Stored_Length) > High_Water_Mark (This)'Old then
           Last = -1 and
           Position (This) = Position (This)'Old + 2
        elsif Stored_Length = 0 then
           --  This is a normal case, specifically for an empty string.
           Last = Value'First - 1 and then
           Position (This) = Position (This)'Old + 2
        else
           --  This is a normal case for a non-empty string in the buffer. If
           --  the stored length is <= Value'Length we only read Stored_Length
           --  number of chars from the buffer, potentially leaving some of
           --  Value unassigned (hence the need for Last).
           Last = Value'First + (Positive (Stored_Length) - 1)               and then
           Position (This) = Position (This)'Old + 2 + Index (Stored_Length) and then
           Equal (Raw_Bytes (This) (Position (This)'Old + 2 .. Position (This) - 1),
                  Value (Value'First .. Last)));

   procedure Get_Unbounded_String
     (This          : in out ByteBuffer'Class;
      Value         : out Unbounded_String;
      Stored_Length : out UInt32)
   with
     Pre  => Remaining (This) >= 2  and then
             Position (This) <= High_Water_Mark (This) - 2,
             --  The string content is preceded in the buffer by a two-byte length,
             --  even when the string length is zero (ie when the string is empty).
     Post => High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Stored_Length <= UInt32 (UInt16'Last)               and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This)'Old + 1) = As_Two_Bytes (UInt16 (Stored_Length)) and then
             (if Index (Stored_Length) > Remaining (This)'Old - 2 or else
                 Position (This)'Old + 2 + Index (Stored_length) > High_Water_Mark (This)
              then
                 To_String (Value) = "Corrupted buffer" and then
                 Position (This) = Position (This)'Old + 2
              elsif Stored_Length = 0 then
                 Position (This) = Position (This)'Old + 2
              else
                 Position (This) = Position (This)'Old + 2 + Index (Stored_Length) and then
                 Equal (Raw_Bytes (This) (Position (This)'Old + 2 .. Position (This) - 1),
                        To_String (Value)));

   procedure Put_Byte (This : in out ByteBuffer'Class;  Value : Byte) with
     Pre  => Remaining (This) >= 1 and then
             High_Water_Mark (This) <= This.Capacity - 1,
     Post => Position (This) = Position (This)'Old + 1               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + 1 and then
             Remaining (This) + 1 = Remaining (This)'Old             and then
             Raw_Bytes (This) (Position (This)'Old) = Value          and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Put_Boolean (This : in out ByteBuffer'Class;  Value : Boolean) with
     Pre  => Remaining (This) >= 1 and then
             High_Water_Mark (This) <= This.Capacity - 1,
     Post => Position (This) = Position (This)'Old + 1                    and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + 1      and then
             Remaining (This) + 1 = Remaining (This)'Old                  and then
             Raw_Bytes (This) (Position (This)'Old) = Boolean'Pos (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Put_Int16 (This : in out ByteBuffer'Class;  Value : Int16) with
     Pre  => Remaining (This) >= 2 and then
             High_Water_Mark (This) <= This.Capacity - 2,
     Post => Position (This) = Position (This)'Old + 2               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + 2 and then
             Remaining (This) + 2 = Remaining (This)'Old             and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Two_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Put_UInt16 (This : in out ByteBuffer'Class;  Value : UInt16) with
     Pre  => Remaining (This) >= 2 and then
             High_Water_Mark (This) <= This.Capacity - 2,
     Post => Position (This) = Position (This)'Old + 2               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + 2 and then
             Remaining (This) + 2 = Remaining (This)'Old             and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Two_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Put_Int32 (This : in out ByteBuffer'Class;  Value : Int32) with
     Pre  => Remaining (This) >= 4 and then
             High_Water_Mark (This) <= This.Capacity - 4,
     Post => Position (This) = Position (This)'Old + 4               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + 4 and then
             Remaining (This) + 4 = Remaining (This)'Old             and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Four_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Put_UInt32 (This : in out ByteBuffer'Class;  Value : UInt32) with
     Pre  => Remaining (This) >= 4 and then
             High_Water_Mark (This) <= This.Capacity - 4,
     Post => Position (This) = Position (This)'Old + 4               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + 4 and then
             Remaining (This) + 4 = Remaining (This)'Old             and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Four_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Put_Int64 (This : in out ByteBuffer'Class;  Value : Int64) with
     Pre  => Remaining (This) >= 8 and then
             High_Water_Mark (This) <= This.Capacity - 8,
     Post => Position (This) = Position (This)'Old + 8               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + 8 and then
             Remaining (This) + 8 = Remaining (This)'Old             and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Eight_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Put_UInt64 (This : in out ByteBuffer'Class;  Value : UInt64) with
     Pre  => Remaining (This) >= 8 and then
             High_Water_Mark (This) <= This.Capacity - 8,
     Post => Position (This) = Position (This)'Old + 8               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + 8 and then
             Remaining (This) + 8 = Remaining (This)'Old             and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Eight_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Put_Real32 (This : in out ByteBuffer'Class;  Value : Real32) with
     Pre  => Remaining (This) >= 4 and then
             High_Water_Mark (This) <= This.Capacity - 4,
     Post => Position (This) = Position (This)'Old + 4               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + 4 and then
             Remaining (This) + 4 = Remaining (This)'Old             and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Four_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Put_Real64 (This : in out ByteBuffer'Class;  Value : Real64) with
     Pre  => Remaining (This) >= 8 and then
             High_Water_Mark (This) <= This.Capacity - 8,
     Post => Position (This) = Position (This)'Old + 8               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + 8 and then
             Remaining (This) + 8 = Remaining (This)'Old             and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = As_Eight_Bytes (Value) and then
             Prior_Content_Unchanged (This, This'Old);

   procedure Put_String (This : in out ByteBuffer'Class;  Value : String) with
     Pre  => Value'First = 1                      and then
             Value'Length <= Max_String_Length    and then
             Remaining (This) >= Value'Length + 2 and then  -- 2 bytes for the length
             High_Water_Mark (This) <= This.Capacity - Value'Length - 2,
     Post => Position (This) = Position (This)'Old + Value'Length + 2               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + Value'Length + 2 and then
             Remaining (This) = Remaining (This)'Old - Value'Length - 2             and then
             New_Content_Equal (This, This.Position'Old + 2, Value)                 and then
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   procedure Put_Unbounded_String (This : in out ByteBuffer'Class; Value : Unbounded_String) with
     Pre  => Length (Value) <= Max_String_Length and then
             Remaining (This) >= Index (Length (Value)) + 2 and then  -- 2 bytes for the length
             High_Water_Mark (This) <= This.Capacity - Index (Length (Value)) - 2,
     Post => Position (This) = Position (This)'Old + Index (Length (Value)) + 2               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + Index (Length (Value)) + 2 and then
             Remaining (This) = Remaining (This)'Old - Index (Length (Value)) - 2             and then
             New_Content_Equal (This, This.Position'Old + 2, To_String (Value))               and then
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

   procedure Put_Raw_Bytes (This : in out ByteBuffer'Class; Value : String) with
     Pre  => Value'Length <= Max_String_Length and then
             Remaining (This) >= Value'Length  and then
             High_Water_Mark (This) + Value'Length <= This.Capacity,
     Post => Position (This) = Position (This)'Old + Value'Length               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + Value'Length and then
             Remaining (This) = Remaining (This)'Old - Value'Length             and then
             New_Content_Equal (This, This.Position'Old, Value)                 and then
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   type Byte_Array is array (Index range <>) of aliased Byte with
     Component_Size => Byte'Size;

   procedure Put_Raw_Bytes (This : in out ByteBuffer'Class; Value : Byte_Array) with
     Pre  => Remaining (This) >= Value'Length and then
             High_Water_Mark (This) + Value'Length <= This.Capacity,
     Post => Position (This) = Position (This)'Old + Value'Length               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + Value'Length and then
             Remaining (This) = Remaining (This)'Old - Value'Length             and then
             New_Content_Equal (This, This.Position'Old, Value)                 and then
             Prior_Content_Unchanged (This, Old_Value => This'Old);

   function Raw_Bytes (This : ByteBuffer'Class) return Byte_Array;
   --  Returns the entire internal byte array content, up to High_Water_Mark (This)

   function Raw_Bytes_As_String (This : ByteBuffer'Class) return String with
     Pre  => High_Water_Mark (This) <= Index (Positive'Last),
     Post => Raw_Bytes_As_String'Result'First = 1 and then
             Raw_Bytes_As_String'Result'Length = High_Water_Mark (This);
   --  Returns the entire internal byte array content, as a String

   function Checksum (This : ByteBuffer'Class;  Last : Index) return UInt32 with
     Pre => Last <= This.Capacity;
   --  Computes the checksum of the slice of the internal byte array from the
   --  first byte up to Last

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

   function Prior_Content_Unchanged (New_Value, Old_Value : ByteBuffer'Class) return Boolean with
     Ghost,
     Pre  => New_Value.Capacity = Old_Value.Capacity;
   --  Returns whether the content of Old_Value was unchanged in New_Value

   function New_Content_Equal
     (This      : ByteBuffer'Class;
      Start     : Index;
      Comparand : String)
   return
      Boolean
   with
     Ghost,
     Pre => Comparand'Length <= Max_String_Length  and then
            This.Capacity >= Comparand'Length      and then
            Start <= Index'Last - Comparand'Length and then
            Start <= This.Capacity - Comparand'Length;
   --  Returns whether the slice of This, starting at Start, equals the
   --  Comparand of type String

   function New_Content_Equal
     (This      : ByteBuffer'Class;
      Start     : Index;
      Comparand : Byte_Array)
   return
      Boolean
   with
     Ghost,
     Pre => Comparand'Length <= Position (This)         and then
            This.Capacity >= Comparand'Length           and then
            Start <= Index'Last - Comparand'Length      and then
            Start <= Position (This) - Comparand'Length and then
            Start <= This.Capacity - Comparand'Length;
   --  Returns whether the slice of This, starting at Start, equals the
   --  Comparand of type Byte_Array

   function Equal (Left : Byte_Array; Right : String) return Boolean with
     Ghost;
   --  Returns whether the individual bytes of the Left Byte_Array, when
   --  considered as Character values, equal the individual characters of
   --  the Right String

private

   type ByteBuffer (Capacity : NonZero_Index) is tagged record
      Content           : Byte_Array (0 .. Capacity) := (others => 0);
      Position          : Index := 0;
      Highest_Write_Pos : Index := 0;
   end record with
      Predicate => Position <= Capacity          and then
                   Highest_Write_Pos <= Capacity and then
                   Position <= Highest_Write_Pos;

   ---------------
   -- Raw_Bytes --
   ---------------

   function Raw_Bytes (This : ByteBuffer'Class) return Byte_Array is
     (if This.Highest_Write_Pos > 0
      then This.Content (0 .. This.Highest_Write_Pos - 1)
      else This.Content (1 .. 0));

   ---------------
   -- Remaining --
   ---------------

   function Remaining (This : ByteBuffer'Class) return Index is
      (This.Capacity - This.Position);

   --------------
   -- Position --
   --------------

   function Position (This : ByteBuffer'Class) return Index is
     (This.Position);

   ---------------------
   -- High_Water_Mark --
   ---------------------

   function High_Water_Mark (This : ByteBuffer'Class) return Index is
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

   pragma Warnings (Off, "Unreachable branch");
   function As_Two_Bytes (Value : Int16) return Two_Bytes is
     (if Standard'Default_Scalar_Storage_Order /= High_Order_First then -- not a Big Endian machine
        To_Two_Bytes (Swapped (Value))
      else
        To_Two_Bytes (Value));
   pragma Warnings (On, "Unreachable branch");

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

   function Prior_Content_Unchanged (New_Value, Old_Value : ByteBuffer'Class) return Boolean is
     (if Old_Value.Position > 0 then  -- not empty, otherwise Unchanged'Result is trivially true
       (New_Value.Content (0 .. Old_Value.Position - 1) = Old_Value.Content (0 .. Old_Value.Position - 1)));

   -----------------------
   -- New_Content_Equal --
   -----------------------

   function New_Content_Equal
     (This      : ByteBuffer'Class;
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
     (This      : ByteBuffer'Class;
      Start     : Index;
      Comparand : Byte_Array)
   return
      Boolean
   is
     (if Comparand'Length > 0 then
         This.Content (Start .. Start + Comparand'Length - 1) = Comparand);

   -----------
   -- Equal --
   -----------

   function Equal (Left : Byte_Array; Right : String) return Boolean is
     (Left'Length = Right'Length and then
      (for all K in Integer range 0 .. Left'Length - 1 =>
          Character'Val (Left (Left'First + Index (K))) = Right (Right'First + K)));

end AVTAS.LMCP.ByteBuffers;
