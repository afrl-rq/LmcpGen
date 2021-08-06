with Conversion_Equality_Lemmas;     use Conversion_Equality_Lemmas;
with AVTAS.LMCP.ByteBuffers.Copying; use AVTAS.LMCP.ByteBuffers.Copying;

package body AVTAS.LMCP.ByteBuffers with
  SPARK_Mode
is

   pragma Unevaluated_Use_Of_Old (Allow);

   ---------------
   -- Raw_Bytes --
   ---------------

   function Raw_Bytes (This : ByteBuffer'Class) return String is
      Length : constant Natural := Natural (Index'Min (Max_String_Length, This.Highest_Write_Pos));
      Result : String (1 .. Length);
   begin
      for K in Result'Range loop
         Result (K) := Character'Val (This.Content (Index (K - 1)));
      end loop;
      return Result;
   end Raw_Bytes;

   ------------
   -- Rewind --
   ------------

   procedure Rewind (This : in out ByteBuffer'Class) is
   begin
      This.Position := 0;
   end Rewind;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out ByteBuffer'Class) is
   begin
      Rewind (This);
      This.Highest_Write_Pos := 0;
   end Reset;

   --------------
   -- Checksum --
   --------------

   function Checksum (This : ByteBuffer'Class;  Last : Index) return UInt32 is
      Result : UInt32 := 0;
   begin
      for K in Index range This.Content'First .. Last loop
         Result := Result + UInt32 (This.Content (K));
         pragma Annotate (GNATProve,
                          Intentional,
                          "overflow check might fail",
                          "Wrap-around semantics are required in this function.");
      end loop;
      return Result;
   end Checksum;

   --------------
   -- Get_Byte --
   --------------

   procedure Get_Byte (This : in out ByteBuffer'Class; Value : out Byte) is
   begin
      Value := This.Content (This.Position);
      This.Position := This.Position + 1;
   end Get_Byte;

   -----------------
   -- Get_Boolean --
   -----------------

   procedure Get_Boolean (This : in out ByteBuffer'Class; Value : out Boolean) is
   begin
      Value := This.Content (This.Position) /= 0;
      This.Position := This.Position + 1;
   end Get_Boolean;

   ---------------
   -- Get_Int16 --
   ---------------

   procedure Get_Int16 (This : in out ByteBuffer'Class; Value : out Int16) is

     procedure Lemma_Two_Bytes_Equal_Int16 is new Lemma_Conversion_Equality
        (Numeric    => Int16,
         Byte       => Byte,
         Index      => Index,
         Bytes      => Byte_Array,
         To_Numeric => As_Int16,
         To_Bytes   => As_Two_Bytes);

   begin
      Value := As_Int16 (This.Content (This.Position .. This.Position + 1));
      Lemma_Two_Bytes_Equal_Int16
        (This_Numeric => Value,
         These_Bytes  => This.Content (This.Position .. This.Position + 1));
      This.Position := This.Position + 2;
   end Get_Int16;

   ----------------
   -- Get_UInt16 --
   ----------------

   procedure Get_UInt16 (This : in out ByteBuffer'Class; Value : out UInt16) is

     procedure Lemma_Two_Bytes_Equal_UInt16 is new Lemma_Conversion_Equality
        (Numeric    => UInt16,
         Byte       => Byte,
         Index      => Index,
         Bytes      => Byte_Array,
         To_Numeric => As_UInt16,
         To_Bytes   => As_Two_Bytes);

   begin
      Value := As_UInt16 (This.Content (This.Position .. This.Position + 1));
      Lemma_Two_Bytes_Equal_UInt16
        (This_Numeric => Value,
         These_Bytes  => This.Content (This.Position .. This.Position + 1));
      This.Position := This.Position + 2;
   end Get_UInt16;

   ---------------
   -- Get_Int32 --
   ---------------

   procedure Get_Int32 (This : in out ByteBuffer'Class; Value : out Int32) is

     procedure Lemma_Four_Bytes_Equal_Int32 is new Lemma_Conversion_Equality
        (Numeric    => Int32,
         Byte       => Byte,
         Index      => Index,
         Bytes      => Byte_Array,
         To_Numeric => As_Int32,
         To_Bytes   => As_Four_Bytes);

   begin
      Value := As_Int32 (This.Content (This.Position .. This.Position + 3));
      Lemma_Four_Bytes_Equal_Int32
        (This_Numeric => Value,
         These_Bytes  => This.Content (This.Position .. This.Position + 3));
      This.Position := This.Position + 4;
   end Get_Int32;

   ----------------
   -- Get_UInt32 --
   ----------------

   procedure Get_UInt32 (This : in out ByteBuffer'Class; Value : out UInt32) is

     procedure Lemma_Four_Bytes_Equal_UInt32 is new Lemma_Conversion_Equality
        (Numeric    => UInt32,
         Byte       => Byte,
         Index      => Index,
         Bytes      => Byte_Array,
         To_Numeric => As_UInt32,
         To_Bytes   => As_Four_Bytes);

   begin
      Value := As_UInt32 (This.Content (This.Position .. This.Position + 3));
      Lemma_Four_Bytes_Equal_UInt32
        (This_Numeric => Value,
         These_Bytes  => This.Content (This.Position .. This.Position + 3));
      This.Position := This.Position + 4;
   end Get_UInt32;

   ----------------
   -- Get_UInt32 --
   ----------------

   procedure Get_UInt32
     (This  : ByteBuffer'Class;
      Value : out UInt32;
      First : Index)
   is

     procedure Lemma_Four_Bytes_Equal_UInt32 is new Lemma_Conversion_Equality
        (Numeric    => UInt32,
         Byte       => Byte,
         Index      => Index,
         Bytes      => Byte_Array,
         To_Numeric => As_UInt32,
         To_Bytes   => As_Four_Bytes);

   begin
      Value := As_UInt32 (This.Content (First .. First + 3));
      Lemma_Four_Bytes_Equal_UInt32
        (This_Numeric => Value,
         These_Bytes  => This.Content (First .. First + 3));
   end Get_UInt32;

   ---------------
   -- Get_Int64 --
   ---------------

   procedure Get_Int64 (This : in out ByteBuffer'Class; Value : out Int64) is

     procedure Lemma_Eight_Bytes_Equal_Int64 is new Lemma_Conversion_Equality
        (Numeric    => Int64,
         Byte       => Byte,
         Index      => Index,
         Bytes      => Byte_Array,
         To_Numeric => As_Int64,
         To_Bytes   => As_Eight_Bytes);

   begin
      Value := As_Int64 (This.Content (This.Position .. This.Position + 7));
      Lemma_Eight_Bytes_Equal_Int64
        (This_Numeric => Value,
         These_Bytes  => This.Content (This.Position .. This.Position + 7));
      This.Position := This.Position + 8;
   end Get_Int64;

   ----------------
   -- Get_UInt64 --
   ----------------

   procedure Get_UInt64 (This : in out ByteBuffer'Class; Value : out UInt64) is

     procedure Lemma_Eight_Bytes_Equal_UInt64 is new Lemma_Conversion_Equality
        (Numeric    => UInt64,
         Byte       => Byte,
         Index      => Index,
         Bytes      => Byte_Array,
         To_Numeric => As_UInt64,
         To_Bytes   => As_Eight_Bytes);

   begin
      Value := As_UInt64 (This.Content (This.Position .. This.Position + 7));
      Lemma_Eight_Bytes_Equal_UInt64
        (This_Numeric => Value,
         These_Bytes  => This.Content (This.Position .. This.Position + 7));
      This.Position := This.Position + 8;
   end Get_UInt64;

   ---------------
   -- Get_Real32 --
   ---------------

   procedure Get_Real32 (This : in out ByteBuffer'Class; Value : out Real32) is

      function To_Real32 is new Ada.Unchecked_Conversion (Source => UInt32, Target => Real32);
      pragma Annotate
        (GNATProve,
         Intentional,
         "type is unsuitable as a target for unchecked conversion",
         "Per the C++ implementation, we assume the same or compatible floating-point" &
         " format on each participating machine");
      --  SPARK does not assume that the UC will produce valid bit
      --  representations for floating-point values.

      function As_Real32 (Value : Four_Bytes) return Real32 is
        (To_Real32 (As_UInt32 (Value)));

     procedure Lemma_Four_Bytes_Equal_Real32 is new Lemma_Conversion_Equality
        (Numeric    => Real32,
         Byte       => Byte,
         Index      => Index,
         Bytes      => Byte_Array,
         To_Numeric => As_Real32,
         To_Bytes   => As_Four_Bytes);

   begin
      --  We get a suitably-sized integer value and then convert to the required
      --  floating-point type, to ensure the compiler does not see the byte
      --  swapping (in the As_UInt32 instance) as producing an invalid
      --  floating-point value.
      Value := As_Real32 (This.Content (This.Position .. This.Position + 3));

      Lemma_Four_Bytes_Equal_Real32
        (This_Numeric => Value,
         These_Bytes  => This.Content (This.Position .. This.Position + 3));

      This.Position := This.Position + 4;
   end Get_Real32;

   ----------------
   -- Get_Real64 --
   ----------------

   procedure Get_Real64 (This : in out ByteBuffer'Class; Value : out Real64) is

      function To_Real64 is new Ada.Unchecked_Conversion (Source => UInt64, Target => Real64);
      pragma Annotate
        (GNATProve,
         Intentional,
         "type is unsuitable as a target for unchecked conversion",
         "Per the C++ implementation, we assume the same or compatible floating-point" &
         " format on each participating machine");
      --  SPARK does not assume that the UC will produce valid bit
      --  representations for floating-point values.

      function As_Real64 (Value : Eight_Bytes) return Real64 is
        (To_Real64 (As_UInt64 (Value)));

      procedure Lemma_Eight_Bytes_Equal_Real64 is new Lemma_Conversion_Equality
        (Numeric    => Real64,
         Byte       => Byte,
         Index      => Index,
         Bytes      => Byte_Array,
         To_Numeric => As_Real64,
         To_Bytes   => As_Eight_Bytes);

   begin
      -- We get a suitably-sized integer value and then convert to the required
      -- floating-point type, to ensure the compiler does not see the byte
      -- swapping (in the As_UInt64 instance) as producing an invalid
      -- floating-point value.
      Value := As_Real64 (This.Content (This.Position .. This.Position + 7));

      Lemma_Eight_Bytes_Equal_Real64
        (This_Numeric => Value,
         These_Bytes  => This.Content (This.Position .. This.Position + 7));

      This.Position := This.Position + 8;
   end Get_Real64;

   ----------------
   -- Get_String --
   ----------------
   procedure Get_String
     (This          : in out ByteBuffer'Class;
      Value         : in out String;
      Last          : out Integer;
      Stored_Length : out UInt32)
   is
      Result : System.Address with Unreferenced;
   begin
      This.Get_UInt16 (UInt16 (Stored_Length));
      if Index (Stored_Length) > Remaining (This) then
         --  We don't have that many bytes logically remaining in the buffer
         --  to be consumed. Some corruption of the message has occurred.
         Last := -1;
      elsif Stored_Length > Value'Length then
         --  The buffer has more string bytes than the arg Value can hold.
         --  Silly client. Nothing wrong with the buffer though.
         Last := -1;
      elsif This.Highest_Write_Pos + Index (Stored_Length) > This.Capacity or else
            This.Position + Index (Stored_Length) > This.Highest_Write_Pos
      then
         Last := -1;
      elsif Stored_Length = 0 then
         --  This is a normal case of an empty string in the buffer.
         Last := Value'First - 1;
      else
         --  The normal, non-zero length case, and the client has passed a
         --  sufficiently large string arg.
         Copy_Buffer_To_String (This, Index (Stored_Length), Value);
         Last := Value'First + (Positive (Stored_Length) - 1);
      end if;
   end Get_String;

   --------------------------
   -- Get_Unbounded_String --
   --------------------------

   procedure Get_Unbounded_String
     (This          : in out ByteBuffer'Class;
      Value         : out Unbounded_String;
      Stored_Length : out UInt32)
   is
   begin
      This.Get_UInt16 (UInt16 (Stored_Length));
      if Stored_Length > Max_String_Length or else
         Index (Stored_Length) > Remaining (This) or else
         This.Position + Index (Stored_length) > This.Highest_Write_Pos
      then
         Value := To_Unbounded_String ("Corrupted buffer");
      elsif Stored_Length = 0 then
         Value := Null_Unbounded_String;
      else
         Copy_Buffer_To_Unbounded_String (This, Index (Stored_Length), Value);
      end if;
   end Get_Unbounded_String;

   --------------
   -- Put_Byte --
   --------------

   procedure Put_Byte (This : in out ByteBuffer'Class; Value : Byte) is
   begin
      This.Content (This.Position) := Value;
      This.Highest_Write_Pos := This.Highest_Write_Pos + 1;
      This.Position := This.Position + 1;
   end Put_Byte;

   -----------------
   -- Put_Boolean --
   -----------------

   procedure Put_Boolean (This : in out ByteBuffer'Class; Value : Boolean) is
   begin
      This.Content (This.Position) := Boolean'Pos (Value);
      This.Highest_Write_Pos := This.Highest_Write_Pos + 1;
      This.Position := This.Position + 1;
   end Put_Boolean;

   ---------------
   -- Put_Int16 --
   ---------------

   procedure Put_Int16 (This : in out ByteBuffer'Class; Value : Int16) is
   begin
      This.Content (This.Position .. This.Position + 1) := As_Two_Bytes (Value);
      This.Highest_Write_Pos := This.Highest_Write_Pos + 2;
      This.Position := This.Position + 2;
   end Put_Int16;

   ----------------
   -- Put_UInt16 --
   ----------------

   procedure Put_UInt16 (This : in out ByteBuffer'Class; Value : UInt16) is
   begin
      This.Content (This.Position .. This.Position + 1) := As_Two_Bytes (Value);
      This.Highest_Write_Pos := This.Highest_Write_Pos + 2;
      This.Position := This.Position + 2;
   end Put_UInt16;

   ---------------
   -- Put_Int32 --
   ---------------

   procedure Put_Int32 (This : in out ByteBuffer'Class; Value : Int32) is
   begin
      This.Content (This.Position .. This.Position + 3) := As_Four_Bytes (Value);
      This.Highest_Write_Pos := This.Highest_Write_Pos + 4;
      This.Position := This.Position + 4;
   end Put_Int32;

   ----------------
   -- Put_UInt32 --
   ----------------

   procedure Put_UInt32 (This : in out ByteBuffer'Class; Value : UInt32) is
   begin
      This.Content (This.Position .. This.Position + 3) := As_Four_Bytes (Value);
      This.Highest_Write_Pos := This.Highest_Write_Pos + 4;
      This.Position := This.Position + 4;
   end Put_UInt32;

   ---------------
   -- Put_Int64 --
   ---------------

   procedure Put_Int64 (This : in out ByteBuffer'Class; Value : Int64) is
   begin
      This.Content (This.Position .. This.Position + 7) := As_Eight_Bytes (Value);
      This.Highest_Write_Pos := This.Highest_Write_Pos + 8;
      This.Position := This.Position + 8;
   end Put_Int64;

   ----------------
   -- Put_UInt64 --
   ----------------

   procedure Put_UInt64 (This : in out ByteBuffer'Class; Value : UInt64) is
   begin
      This.Content (This.Position .. This.Position + 7) := As_Eight_Bytes (Value);
      This.Highest_Write_Pos := This.Highest_Write_Pos + 8;
      This.Position := This.Position + 8;
   end Put_UInt64;

   ----------------
   -- Put_Real32 --
   ----------------

   procedure Put_Real32 (This : in out ByteBuffer'Class; Value : Real32) is
   begin
      This.Content (This.Position .. This.Position + 3) := As_Four_Bytes (Value);
      This.Highest_Write_Pos := This.Highest_Write_Pos + 4;
      This.Position := This.Position + 4;
  end Put_Real32;

   ----------------
   -- Put_Real64 --
   ----------------

   procedure Put_Real64 (This : in out ByteBuffer'Class; Value : Real64) is
   begin
      This.Content (This.Position .. This.Position + 7) := As_Eight_Bytes (Value);
      This.Highest_Write_Pos := This.Highest_Write_Pos + 8;
      This.Position := This.Position + 8;
  end Put_Real64;

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (This : in out ByteBuffer'Class;  Value : String) is
   begin
      --  We need to insert the length in any case, including when zero, so that
      --  the deserialization routine will have a length to read. That routine
      --  will then read that many bytes, so a zero length will work on that
      --  side.
      This.Put_UInt16 (Value'Length);
      if Value'Length > 0 then
         This.Put_Raw_Bytes (Value);
      end if;
   end Put_String;

   -------------------
   -- Put_Raw_Bytes --
   -------------------

   procedure Put_Raw_Bytes (This : in out ByteBuffer'Class; Value : String) is
   begin
      if Value'Length > 0 then
         Copy_String_To_Buffer (This, From => Value);
      end if;
   end Put_Raw_Bytes;

   -------------------
   -- Put_Raw_Bytes --
   -------------------

   procedure Put_Raw_Bytes (This : in out ByteBuffer'Class; Value : Byte_Array) is
   begin
      if Value'Length > 0 then
         Copy_Bytes_To_Buffer (This, From => Value);
      end if;
   end Put_Raw_Bytes;

   --------------------------
   -- Put_Unbounded_String --
   --------------------------

   procedure Put_Unbounded_String (This : in out ByteBuffer'Class;  Value : Unbounded_String) is
   begin
      This.Put_String (To_String (Value));
   end Put_Unbounded_String;

end AVTAS.LMCP.ByteBuffers;
