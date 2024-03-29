with AVTAS.LMCP.ByteBuffers; use AVTAS.LMCP.ByteBuffers;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Assertions;         use Ada.Assertions;
with Ada.Strings.Unbounded;
with AVTAS.LMCP.Types;       use AVTAS.LMCP.Types;
with Ada.Unchecked_Deallocation;

procedure Prove_ByteBuffers with
   SPARK_Mode
is
   package ASU renames Ada.Strings.Unbounded;
begin

   pragma Warnings (Off, "has no effect");
   pragma Warnings (Off, "not used after the call");

--  get from relative index after rewind -------------------------------------

   declare
      C : constant := 100; -- arbitrary
      Buffer : ByteBuffer (Capacity => C);
      V : UInt16;
   begin
      Put_Line ("Get (first) value after rewind");
      Put_Uint16 (Buffer, 42);
      Rewind (Buffer);
      Get_UInt16 (Buffer, V);
      pragma Assert (V = 42);
   end;

--  get from absolute index --------------------------------------------------

   declare
      --                                  123456789ABC
      String_Input  : constant String := "Hello World!";
      Index_After_String : constant := 2 + String_Input'Length;
      --  the 2-byte length of the string precedes the actual string content so
      --  we add 2

      UInt32_Input  : constant UInt32 := 42;
      Byte_Input    : constant Byte := 42;

      Expected_High_Water_Mark : constant := 2 + String_Input'Length + 4 + 1;
      --  2-bytes for string length + length of string + 4 bytes for uint32 + 1 byte for boolean

      Buffer : ByteBuffer (Capacity => 100);
   begin
      Put_Line ("Get_UInt32 from absolute index > position and <= high water mark");
      Put_String (Buffer, String_Input);
      Put_UInt32 (Buffer, UInt32_Input);
      Put_Byte (Buffer, Byte_Input);

      Rewind (Buffer);

      Assert (Position (Buffer) = 0);
      Assert (High_Water_Mark (Buffer) = Expected_High_Water_Mark);

      --  now we read back one of the written values at an absolute position
      --  that is greater than the current position but not greater than the
      --  high water mark, which should succeed
      declare
         Output : UInt32;
      begin
         Get_UInt32 (Buffer, Output, First => Index_After_String);
         pragma Assert (Output = UInt32_Input);
      end;
   end;

--  inserting strings --------------------------------------------------------

   declare
      C : constant := 100; -- arbitrary
      Buffer : ByteBuffer (Capacity => C);
      L : constant := C - 2;  -- leave room for length inserted into buffer too
      S : constant String (1 .. L) := (others => 'x');
   begin
      Put_Line ("Inserting string with length < capacity");
      Put_String (Buffer, S);
   end;

   declare
      C : constant := UInt16'Last;
      Buffer : ByteBuffer (Capacity => C + 2);
      S : constant String (1 .. Integer (UInt16'Last)) := (others => 'x');
   begin
      Put_Line ("Inserting string with length = UInt16'Last, with sufficient capacity");
      Put_String (Buffer, S);
   end;

--  getting Strings ----------------------------------------------------------

   declare
      C : constant := 100; -- arbitrary
      Buffer : ByteBuffer (Capacity => C);
      Corrupted_String_Length : constant := C + 1; -- anything > C will do
      S : String (1 .. Corrupted_String_Length) := (others => ' ');
      Last : Integer;
      Unused : UInt32;
   begin
      Put_Line ("Getting string with stored length > remaining bytes in message");
      --  Write a value that Get_String is going to read as the length of the
      --  string in the message. The value must be > buffer capacity.
      Put_UInt16 (Buffer, Corrupted_String_Length);
      --  Prepare to start getting values as if a message is in the buffer,
      --  starting with a string. The actual characters are immaterial so we
      --  don't bother to insert them into the
      --
      --  Note that this wouldn't happen without some sort of buffer
      --  corruption because Put_String would have failed when attempting to
      --  write that any chars into the buffer (since we know the buffer isn't
      --  big enough in this test, on purpose).

      Rewind (Buffer);
      Get_String (Buffer, S, Last, Unused);
      pragma Assert (Position (Buffer) = 2);
      pragma Assert (Last = -1);
   end;

   -- the case in which stored length > string arg length AND we just set Last to -1
   declare
      C : constant := 10; -- arbitrary
      Buffer : ByteBuffer (Capacity => C);
      Written : constant String (1 .. 8) := "helloyou";
      Read : String (1 .. 5) := (others => ' ');
      Last : Integer;
      Stored_Length : UInt32;
   begin
      Put_Line ("Getting string with stored length > arg length");
      Put_String (Buffer, Written);
      Rewind (Buffer);
      Get_String (Buffer, Read, Last, Stored_Length);

      pragma Assert (Last = -1);
      pragma Assert (Position (Buffer) = 2);
   end;

   declare
      C : constant := 100; -- arbitrary
      Buffer : ByteBuffer (Capacity => C);
      Written : constant String (1 .. 5) := "world";
      Read : String (1 .. 10) := (others => ' ');
      Last : Integer;
      Unused : UInt32;
   begin
      Put_Line ("Getting string with stored length < arg length");
      Put_String (Buffer, Written);
      --  prepare to start getting values as if a message is in the buffer,
      --  starting with a string
      Rewind (Buffer);
      Get_String (Buffer, Read, Last, Unused);
      pragma Assert (Last = Written'Length);
      pragma Assert (Read (1 .. Last) = Written);
   end;

--  writing unbounded strings -------------------------------------------------

   declare
      C : constant := 100; -- arbitrary
      L : constant := C - 2;  -- leave room for length inserted into buffer too
      Buffer : ByteBuffer (Capacity => C);
      S : constant ASU.Unbounded_String := ASU.To_Unbounded_String (Source => String'(1 .. L => 'x'));
   begin
      Put_Line ("Inserting unbounded string with length < capacity");
      Put_Unbounded_String (Buffer, S);
   end;

   declare
      C : constant := 2;  -- the 2 bytes for the string's bounds
      Buffer : ByteBuffer (Capacity => C);
      L : constant := 0;
      S : constant ASU.Unbounded_String := ASU.To_Unbounded_String (Source => String'(1 .. L => 'x'));
   begin
      Put_Line ("Inserting unbounded string with length = 0, with sufficient capacity");
      Put_Unbounded_String (Buffer, S);
   end;

   declare
      C : constant := 100; -- arbitrary
      L : constant := C - 2;  -- leave room for length inserted into buffer too
      Buffer : ByteBuffer (Capacity => C);
      S : constant ASU.Unbounded_String := ASU.To_Unbounded_String (Source => String'(1 .. L => 'x'));
   begin
      Put_Line ("Inserting unbounded string with length < capacity");
      Put_Unbounded_String (Buffer, S);
   end;

--  reading unbounded strings -------------------------------------------------

   declare
      C : constant := Max_String_Length;
      Buffer : ByteBuffer (Capacity => C + 2);
      L : constant := Integer (C);
      S : constant ASU.Unbounded_String := ASU.To_Unbounded_String (Source => String'(1 .. L => 'x'));
      O : ASU.Unbounded_String := ASU.To_Unbounded_String (Length => L);
      Num_Stored : UInt32;
      use ASU;
   begin
      Put_Line ("Getting unbounded string with length = Max_String_Length, with sufficient capacity");
      Put_Unbounded_String (Buffer, S);
      Rewind (Buffer);
      Get_Unbounded_String (Buffer, O, Num_Stored);
      pragma Assert (Num_Stored = Max_String_Length);
      pragma Assert (O = S);
   end;

--  raw bytes ----------------------------------------------------------------

   declare
      C : constant := 100; -- arbitrary
      Buffer : ByteBuffer (Capacity => C);
      L : constant := C - 2;  -- leave room for length inserted into buffer too
      S : constant String (1 .. L) := (others => 'x');
   begin
      Put_Line ("Inserting raw bytes from String with length < capacity");
      Put_Raw_Bytes (Buffer, S);
   end;

   declare
      C : constant := 100; -- arbitrary
      Buffer : ByteBuffer (Capacity => C);
      L : constant := C - 2;  -- leave room for length inserted into buffer too
      S : constant Byte_Array (1 .. L) := (others => Character'Pos ('x'));
   begin
      Put_Line ("Inserting raw bytes with length < capacity");
      Put_Raw_Bytes (Buffer, S);
   end;

   declare
      C : constant := UInt32 (Positive'Last) + 10;
      type ByteBuffer_Pointer is access ByteBuffer;
      BBP : ByteBuffer_Pointer := new ByteBuffer (C);

      L : constant := Positive'Last;
      subtype Constrained_Byte_Array is Byte_Array (1 .. L);
      type Byte_Array_Pointer is access Constrained_Byte_Array;
      BAP : Byte_Array_Pointer := new Byte_Array'(1 .. L => Character'Pos ('x'));

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => ByteBuffer, Name => ByteBuffer_Pointer);
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Constrained_Byte_Array, Name => Byte_Array_Pointer);
   begin
      Put_Line ("Inserting raw bytes with length = Positive'Last");
      Put_Raw_Bytes (BBP.all, BAP.all);
      --  and free them to keep SPARK happy
      Free (BAP);
      Free (BBP);
   end;

--  sequence of writes followed by sequence of reads  ------------------------

   declare
      Byte_Input    : constant Byte := 42;
      String_Input  : constant String := "Hello World!";
      UInt32_Input  : constant UInt32 := 42;
      Real32_Input  : constant Real32 := 42.42;
      Boolean_Input : constant Boolean := True;
      UInt64_Input  : constant UInt64 := 84;

      Buffer : ByteBuffer (Capacity => 1024);
   begin
      Put_Line ("Multiple writes folowed by reads of those written values");

      --  NB: the order of the following must match the order of the calls to Get_* below
      Put_Byte (Buffer, Byte_Input);
      Put_String (Buffer, String_Input);
      Put_UInt32 (Buffer, UInt32_Input);
      Put_Unbounded_String (Buffer, ASU.To_Unbounded_String (String_Input));
      Put_Real32 (Buffer, Real32_Input);
      Put_Boolean (Buffer, Boolean_Input);
      Put_UInt64 (Buffer, UInt64_Input);

      --  now we read back what was written

      Rewind (Buffer);

      declare
         Output : Byte;
      begin
         Get_Byte (Buffer, Output);
         pragma Assert (Output = Byte_Input);
      end;

      declare
         Output : String (String_Input'Range) := (others => ' ');
         Last   : Integer;  -- because result can be -1 to signal a problem
         Unused : UInt32;
      begin
         Get_String (Buffer, Output, Last, Unused);
         pragma Assert (Last = String_Input'Length);
         pragma Assert (Output (1 .. Last) = String_Input);
      end;

      declare
         Output : UInt32;
      begin
         Get_UInt32 (Buffer, Output);
         pragma Assert (Output = UInt32_Input);
      end;

      declare
         Output : ASU.Unbounded_String;
         Num_Stored : UInt32;
      begin
         Get_Unbounded_String (Buffer, Output, Num_Stored);
         pragma Assert (Num_Stored = String_Input'Length);
         pragma Assert (ASU.To_String (Output) = String_Input);
      end;

      declare
         Output : Real32;
      begin
         Get_Real32 (Buffer, Output);
         pragma Assert (Output = Real32_Input);
      end;

      declare
         Output : Boolean;
      begin
         Get_Boolean (Buffer, Output);
         pragma Assert (Output = Boolean_Input);
      end;

      declare
         Output : UInt64;
      begin
         Get_UInt64 (Buffer, Output);
         pragma Assert (Output = UInt64_Input);
      end;
   end;

   Put_Line ("Done");  -- just in case you actually want to run this
end Prove_ByteBuffers;
