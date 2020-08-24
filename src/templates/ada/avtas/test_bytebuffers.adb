with AVTAS.LMCP.ByteBuffers; use AVTAS.LMCP.ByteBuffers;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Assertions;         use Ada.Assertions;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Exceptions;         use Ada.Exceptions;
with AVTAS.LMCP.Types;       use AVTAS.LMCP.Types;

procedure Test_ByteBuffers is
begin
   New_Line;

--  strings ------------------------------------------------------------------

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      S : constant String (1 .. C + 1) := (others => 'x');
   begin
      Put ("Inserting string with length > capacity ");
      B.Put_String (S);
      Put_Line ("failed (should have raised Assertion_Error)");
   exception
      when Assertion_Error =>
         Put_Line ("passed (raised Assertion_Error)");
      when Error : others =>
         Put_Line (Exception_Information (Error));
   end;

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      L : constant := C - 2;  -- leave room for length inserted into buffer too
      S : constant String (1 .. L) := (others => 'x');
   begin
      Put ("Inserting string with length < capacity ");
      B.Put_String (S);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
   end;

   declare
      C : constant := UInt16'Last;
      B : ByteBuffer (Capacity => C + 2);
      S : constant String (1 .. Integer (UInt16'Last) + 1) := (others => 'x');
   begin
      Put ("Inserting string with length > UInt16'Last, with sufficient capacity ");
      B.Put_String (S);
      Put_Line ("failed (should have raised Assertion_Error)");
   exception
      when Assertion_Error =>
         Put_Line ("passed (raised Assertion_Error)");
      when Error : others =>
         Put_Line (Exception_Information (Error));
   end;

   declare
      C : constant := UInt16'Last;
      B : ByteBuffer (Capacity => C + 2);
      S : constant String (1 .. Integer (UInt16'Last)) := (others => 'x');
   begin
      Put ("Inserting string with length = UInt16'Last, with sufficient capacity ");
      B.Put_String (S);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
   end;

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      S : String (1 .. 1);
      Last : Natural;
   begin
      Put ("Getting string with indicated length > remaining bytes in message ");
      --  write a value that Get_String is going to read as the length of the
      --  string in the message. The actual characters are immaterial so we
      --  don't bother to insert them into the buffer.
      B.Put_UInt16 (C + 2);
      --  prepare to start getting values as if a message is in the buffer,
      --  starting with a string
      B.Rewind;
      --  we wrote the UInt16 value as if it is the length of bytes to be
      --  read for the string, so the following Get_String call should raise
      --  the specific exception
      B.Get_String (S, Last);
      Put_Line ("failed (should have raised Runtime_Length_Error)");
   exception
      when Runtime_Length_Error =>
         Put_Line ("passed (raised Runtime_Length_Error)");
      when Error : others =>
         Put_Line (Exception_Information (Error));
   end;

--  unbounded strings ------------------------------------------------------------------

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      L : constant := C + 1;
      S : constant Unbounded_String := To_Unbounded_String (Source => String'(1 .. L => 'x'));
   begin
      Put ("Inserting unbounded string with length > capacity ");
      B.Put_Unbounded_String (S);
      Put_Line ("failed (should have raised Assertion_Error)");
   exception
      when Assertion_Error =>
         Put_Line ("passed (raised Assertion_Error)");
      when Error : others =>
         Put_Line (Exception_Information (Error));
   end;

   declare
      C : constant := 100; -- arbitrary
      L : constant := C - 2;  -- leave room for length inserted into buffer too
      B : ByteBuffer (Capacity => C);
      S : constant Unbounded_String := To_Unbounded_String (Source => String'(1 .. L => 'x'));
   begin
      Put ("Inserting unbounded string with length < capacity ");
      B.Put_Unbounded_String (S);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
   end;

   declare
      C : constant := UInt16'Last;
      B : ByteBuffer (Capacity => C + 2);
      L : constant := Integer (C) + 1;
      S : constant Unbounded_String := To_Unbounded_String (Source => String'(1 .. L => 'x'));
   begin
      Put ("Inserting unbounded string with length > UInt16'Last, with sufficient capacity ");
      B.Put_Unbounded_String (S);
      Put_Line ("failed (should have raised Assertion_Error)");
   exception
      when Assertion_Error =>
         Put_Line ("passed (raised Assertion_Error)");
      when Error : others =>
         Put_Line (Exception_Information (Error));
   end;

   declare
      C : constant := UInt16'Last;
      B : ByteBuffer (Capacity => C + 2);
      L : constant := Integer (C);
      S : constant Unbounded_String := To_Unbounded_String (Source => String'(1 .. L => 'x'));
   begin
      Put ("Inserting unbounded string with length = UInt16'Last, with sufficient capacity ");
      B.Put_Unbounded_String (S);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
   end;

--  raw bytes ------------------------------------------------------------------

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      S : constant String (1 .. C + 1) := (others => 'x');
   begin
      Put ("Inserting raw bytes with length > capacity ");
      B.Put_Raw_Bytes (S);
      Put_Line ("failed (should have raised Assertion_Error)");
   exception
      when Assertion_Error =>
         Put_Line ("passed (raised Assertion_Error)");
      when Error : others =>
         Put_Line (Exception_Information (Error));
   end;

   declare
      C : constant := 100; -- arbitrary
      B : ByteBuffer (Capacity => C);
      L : constant := C - 2;  -- leave room for length inserted into buffer too
      S : constant String (1 .. L) := (others => 'x');
   begin
      Put ("Inserting raw bytes with length < capacity ");
      B.Put_Raw_Bytes (S);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
   end;

   declare
      B : ByteBuffer (Capacity => UInt32 (UInt16'Last) + 2);
      S : constant String (1 .. Integer (UInt16'Last) + 1) := (others => 'x');
   begin
      Put ("Inserting raw bytes with length > UInt16'Last, with sufficient capacity ");
      B.Put_Raw_Bytes (S);
      Put_Line ("passed");
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
   end;

--  done ------------------------------------------------------------------

   New_Line;
   Put_Line ("ByteBuffer testing complete");
end Test_ByteBuffers;
