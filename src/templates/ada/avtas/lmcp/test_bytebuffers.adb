with AVTAS.LMCP.ByteBuffers; use AVTAS.LMCP.ByteBuffers;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Assertions;         use Ada.Assertions;
with Ada.Strings.Unbounded;  --  use Ada.Strings.Unbounded;
with Ada.Exceptions;         use Ada.Exceptions;
with AVTAS.LMCP.Types;       use AVTAS.LMCP.Types;

procedure Test_ByteBuffers is
   Failed : Natural := 0;

   package ASU renames Ada.Strings.Unbounded;
begin

--  get from relative index --------------------------------------------------

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      V : UInt16 := 42;
   begin
      Put ("Get (first) value after rewind: ");
      B.Put_Uint16 (V);
      B.Rewind;
      V := 0;
      B.Get_UInt16 (V);
      if V = 42 then
         Put_Line ("passed");
      else
         Put_Line ("failed, wrong value read");
      end if;
   exception
      when Assertion_Error =>
         Put_Line ("FAILED (raised Assertion_Error)");
         Failed := Failed + 1;
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
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
      Put ("Get_UInt32 from absolute index > position and <= high water mark: ");
      Buffer.Put_String (String_Input);
      Buffer.Put_UInt32 (UInt32_Input);
      Buffer.Put_Byte (Byte_Input);

      Buffer.Rewind;

      Assert (Position (Buffer) = 0, "Invalid position in test Get_UInt32 absolute");
      Assert (High_Water_Mark (Buffer) = Expected_High_Water_Mark, "Invalid high water mark in test Get_UInt32 absolute");

      --  now we read back one of the written values at an absolute position
      --  that is greater than the current position but not greater than the
      --  high water mark, which should succeed
      declare
         Output : UInt32;
      begin
         Buffer.Get_UInt32 (Output, First => Index_After_String);
         if Output = UInt32_Input then
            Put_Line ("passed");
         else
            Put_Line ("FAILED (value read /= value written)");
            Failed := Failed + 1;
         end if;
      end;
   exception
      when Error : others =>
         Put_Line (Exception_Message (Error));
         Failed := Failed + 1;
   end;

   declare
      String_Input  : constant String := "Hello World!";

      Expected_High_Water_Mark : constant := 2 + String_Input'Length;
      --  2-bytes for string length + length of string

      Invalid_Index : constant Index := Expected_High_Water_Mark + 1;

      Buffer : ByteBuffer (Capacity => 100);
   begin
      Put ("Get_UInt32 from absolute index > position and > high water mark: ");
      Buffer.Put_String (String_Input);

      Buffer.Rewind;

      Assert (Position (Buffer) = 0, "Invalid position in test Get_UInt32 absolute > high water mark");
      Assert (High_Water_Mark (Buffer) = Expected_High_Water_Mark,
              "Invalid high water mark in test Get_UInt32 absolute > high water mark");

      --  now we read back from an index greater than the high water mark, which should fail
      declare
         Output : UInt32;
      begin
         Buffer.Get_UInt32 (Output, First => Invalid_Index);
         Put_Line ("FAILED (should have raised Assertion_Error)");
         Failed := Failed + 1;
      exception
         when Assertion_Error =>
            Put_Line ("passed");
      end;
   exception
      when Error : others =>
         Put_Line (Exception_Message (Error));
         Failed := Failed + 1;
   end;

--  strings ------------------------------------------------------------------

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      S : constant String (1 .. C + 1) := (others => 'x');
   begin
      Put ("Inserting string with length > capacity: ");
      B.Put_String (S);
      Put_Line ("FAILED (should have raised Assertion_Error)");
      Failed := Failed + 1;
   exception
      when Assertion_Error =>
         Put_Line ("passed");
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      L : constant := C - 2;  -- leave room for length inserted into buffer too
      S : constant String (1 .. L) := (others => 'x');
   begin
      Put ("Inserting string with length < capacity: ");
      B.Put_String (S);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   declare
      C : constant := UInt16'Last;
      B : ByteBuffer (Capacity => C + 2);
      S : constant String (1 .. Integer (UInt16'Last) + 1) := (others => 'x');
   begin
      Put ("Inserting string with length > UInt16'Last, with sufficient capacity: ");
      B.Put_String (S);
      Put_Line ("FAILED (should have raised Assertion_Error)");
      Failed := Failed + 1;
   exception
      when Assertion_Error =>
         Put_Line ("passed");
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   declare
      C : constant := UInt16'Last;
      B : ByteBuffer (Capacity => C + 2);
      S : constant String (1 .. Integer (UInt16'Last)) := (others => 'x');
   begin
      Put ("Inserting string with length = UInt16'Last, with sufficient capacity: ");
      B.Put_String (S);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

--  getting Strings ----------------------------------------------------------

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      Corrupted_String_Length : constant := C + 1; -- anything > C will do
      S : String (1 .. Corrupted_String_Length);
      Last : Integer;
      Unused : UInt32;
   begin
      Put ("Getting string with stored length > remaining bytes in message: ");
      --  Write a value that Get_String is going to read as the length of the
      --  string in the message. The value must be > buffer capacity.
      B.Put_UInt16 (Corrupted_String_Length);
      --  Prepare to start getting values as if a message is in the buffer,
      --  starting with a string. The actual characters are immaterial so we
      --  don't bother to insert them into the buffer.
      --
      --  Note that this wouldn't happen without some sort of buffer
      --  corruption because Put_String would have failed when attempting to
      --  write that any chars into the buffer (since we know the buffer isn't
      --  big enough in this test, on purpose).

      B.Rewind;
      B.Get_String (S, Last, Unused);
      pragma Assert (B.Position = 2);
      pragma Assert (Last = -1);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   -- the case in which stored length > string arg length AND we just set Last to -1
   declare
      C : constant := 10; -- arbitrary
      B : ByteBuffer (Capacity => C);
      Written : constant String (1 .. 8) := "helloyou";
      Read : String (1 .. 5) := (others => ' ');
      Last : Integer;
      Stored_Length : UInt32;
   begin
      Put ("Getting string with stored length > arg length: ");
      B.Put_String (Written);
      B.Rewind;
      B.Get_String (Read, Last, Stored_Length);

      if Last /= -1 then
         Put_Line ("failed (incorrect value for Last)");
         Failed := Failed + 1;
      elsif B.Position /= 2 then
         Put_Line ("failed (wrong Position)");
         Failed := Failed + 1;
      else
         Put_Line ("passed");
      end if;
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      Written : constant String (1 .. 5) := "world";
      Read : String (1 .. 10) := (others => ' ');
      Last : Integer;
      Unused : UInt32;
   begin
      Put ("Getting string with stored length < arg length: ");
      B.Put_String (Written);
      --  prepare to start getting values as if a message is in the buffer,
      --  starting with a string
      B.Rewind;
      B.Get_String (Read, Last, Unused);

      if Last /= Written'Length then
         Put_Line ("failed (incorrect value for Last)");
         Failed := Failed + 1;
      elsif Read (1 .. Last) = Written then
         Put_Line ("passed");
      else
         Put_Line ("failed (incorrect value read)");
         Failed := Failed + 1;
      end if;
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

--  writing unbounded strings -------------------------------------------------

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      L : constant := C + 1;
      S : constant ASU.Unbounded_String := ASU.To_Unbounded_String (Source => String'(1 .. L => 'x'));
   begin
      Put ("Inserting unbounded string with length > capacity: ");
      B.Put_Unbounded_String (S);
      Put_Line ("FAILED (should have raised Assertion_Error)");
      Failed := Failed + 1;
   exception
      when Assertion_Error =>
         Put_Line ("passed");
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   declare
      C : constant := 100; -- arbitrary
      L : constant := C - 2;  -- leave room for length inserted into buffer too
      B : ByteBuffer (Capacity => C);
      S : constant ASU.Unbounded_String := ASU.To_Unbounded_String (Source => String'(1 .. L => 'x'));
   begin
      Put ("Inserting unbounded string with length < capacity: ");
      B.Put_Unbounded_String (S);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   declare
      C : constant := UInt16'Last;
      B : ByteBuffer (Capacity => C + 2);
      L : constant := Integer (C) + 1;
      S : constant ASU.Unbounded_String := ASU.To_Unbounded_String (Source => String'(1 .. L => 'x'));
   begin
      Put ("Inserting unbounded string with length > UInt16'Last, with sufficient capacity: ");
      B.Put_Unbounded_String (S);
      Put_Line ("FAILED (should have raised Assertion_Error)");
      Failed := Failed + 1;
   exception
      when Assertion_Error =>
         Put_Line ("passed");
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   declare
      C : constant := 2;  -- the 2 bytes for the string's bounds
      B : ByteBuffer (Capacity => C);
      L : constant := 0;
      S : constant ASU.Unbounded_String := ASU.To_Unbounded_String (Source => String'(1 .. L => 'x'));
   begin
      Put ("Inserting unbounded string with length = 0, with sufficient capacity: ");
      B.Put_Unbounded_String (S);
      Put_Line ("passed");
   exception
      when Assertion_Error =>
         Put_Line ("FAILED (raised Assertion_Error)");
         Failed := Failed + 1;
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   declare
      C : constant := 1;  -- less than the 2 bytes for the string's bounds
      B : ByteBuffer (Capacity => C);
      L : constant := 0;
      S : constant ASU.Unbounded_String := ASU.To_Unbounded_String (Source => String'(1 .. L => 'x'));
   begin
      Put ("Inserting unbounded string with length = 0, without sufficient capacity: ");
      B.Put_Unbounded_String (S);
      Put_Line ("FAILED");  -- we didn't have room for the bounds
      Failed := Failed + 1;
   exception
      when Assertion_Error =>
         Put_Line ("passed");
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   declare
      C : constant := 100; -- arbitrary
      L : constant := C - 2;  -- leave room for length inserted into buffer too
      B : ByteBuffer (Capacity => C);
      S : constant ASU.Unbounded_String := ASU.To_Unbounded_String (Source => String'(1 .. L => 'x'));
   begin
      Put ("Inserting unbounded string with length < capacity: ");
      B.Put_Unbounded_String (S);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

--  reading unbounded strings -------------------------------------------------

   declare
      C : constant := Max_String_Length;
      B : ByteBuffer (Capacity => C + 2);
      L : constant := Integer (C);
      S : constant ASU.Unbounded_String := ASU.To_Unbounded_String (Source => String'(1 .. L => 'x'));
      O : ASU.Unbounded_String := ASU.To_Unbounded_String (Length => L);
      Num_Stored : UInt32;
      use ASU;
   begin
      Put ("Getting unbounded string with length = Max_String_Length, with sufficient capacity: ");
      B.Put_Unbounded_String (S);
      B.Rewind;
      B.Get_Unbounded_String (O, Num_Stored);
      if Num_Stored /= Max_String_Length then
         Put_Line ("failed (num stored mismatch)");
         Failed := Failed + 1;
      elsif O /= S then
         Put_Line ("failed (content mismatch)");
         Failed := Failed + 1;
      else
         Put_Line ("passed");
      end if;
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

--  raw bytes ----------------------------------------------------------------

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      S : constant String (1 .. C + 1) := (others => 'x');
   begin
      Put ("Inserting raw bytes from String with length > capacity: ");
      B.Put_Raw_Bytes (S);
      Put_Line ("FAILED (should have raised Assertion_Error)");
      Failed := Failed + 1;
   exception
      when Assertion_Error =>
         Put_Line ("passed");
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      L : constant := C - 2;  -- leave room for length inserted into buffer too
      S : constant String (1 .. L) := (others => 'x');
   begin
      Put ("Inserting raw bytes from String with length < capacity: ");
      B.Put_Raw_Bytes (S);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   declare
      B : ByteBuffer (Capacity => Index (UInt16'Last) + 2);
      S : constant String (1 .. Integer (UInt16'Last) + 1) := (others => 'x');
   begin
      Put ("Inserting raw bytes from String with length > UInt16'Last, with sufficient capacity: ");
      B.Put_Raw_Bytes (S);
      Put_Line ("failed");
      Failed := Failed + 1;
   exception
      when Assertion_Error =>
         Put_Line ("passed");
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      L : constant := C - 2;  -- leave room for length inserted into buffer too
      S : constant Byte_Array (1 .. L) := (others => Character'Pos ('x'));
   begin
      Put ("Inserting raw bytes with length < capacity: ");
      B.Put_Raw_Bytes (S);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
   end;

   declare
      C : constant := UInt32 (Positive'Last) + 10;
      type ByteBuffer_Pointer is access ByteBuffer;
      BBP : constant ByteBuffer_Pointer := new ByteBuffer (C);
      L : constant := Positive'Last;
      type Byte_Array_Pointer is access Byte_Array (1 .. L);
      BAP : constant Byte_Array_Pointer := new Byte_Array'(1 .. L => Character'Pos ('x'));
   begin
      Put ("Inserting raw bytes with length = Positive'Last: ");
      BBP.Put_Raw_Bytes (BAP.all);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Failed := Failed + 1;
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
      Put ("Multiple writes folowed by reads of those written values: ");

      --  NB: the order of the following must match the order of the calls to Get_* below
      Buffer.Put_Byte (Byte_Input);
      Buffer.Put_String (String_Input);
      Buffer.Put_UInt32 (UInt32_Input);
      Buffer.Put_Unbounded_String (ASU.To_Unbounded_String (String_Input));
      Buffer.Put_Real32 (Real32_Input);
      Buffer.Put_Boolean (Boolean_Input);
      Buffer.Put_UInt64 (UInt64_Input);

      --  now we read back what was written

      Buffer.Rewind;

      declare
         Output : Byte;
      begin
         Buffer.Get_Byte (Output);
         pragma Assert (Output = Byte_Input, "Getting Byte failed");
      end;

      declare
         Output : String (String_Input'Range);
         Last   : Natural;
         Unused : UInt32;
      begin
         Buffer.Get_String (Output, Last, Unused);
         pragma Assert (Last = String_Input'Length, "Getting string Last failed");
         pragma Assert (Output (1 .. Last) = String_Input, "Getting string failed");
      end;

      declare
         Output : UInt32;
      begin
         Buffer.Get_UInt32 (Output);
         pragma Assert (Output = UInt32_Input, "Getting UInt32 failed");
      end;

      declare
         Output : ASU.Unbounded_String;
         Num_Stored : UInt32;
      begin
         Buffer.Get_Unbounded_String (Output, Num_Stored);
         pragma Assert (Num_Stored = String_Input'Length,  "Getting Unbounded_String failed (stored count mismatch)");
         pragma Assert (ASU.To_String (Output) = String_Input, "Getting Unbounded_String failed (content mismatch)");
      end;

      declare
         Output : Real32;
      begin
         Buffer.Get_Real32 (Output);
         pragma Assert (Output = Real32_Input, "Getting Real32 failed");
      end;

      declare
         Output : Boolean;
      begin
         Buffer.Get_Boolean (Output);
         pragma Assert (Output = Boolean_Input, "Getting Boolean failed");
      end;

      declare
         Output : UInt64;
      begin
         Buffer.Get_UInt64 (Output);
         pragma Assert (Output = UInt64_Input, "Getting UInt64 failed");
      end;

      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Message (Error));
         Failed := Failed + 1;
   end;

--  done ---------------------------------------------------------------------

   New_Line;
   Put_Line ("Failed Tests:" & Failed'Image);
   Put_Line ("ByteBuffer testing complete");
end Test_ByteBuffers;
