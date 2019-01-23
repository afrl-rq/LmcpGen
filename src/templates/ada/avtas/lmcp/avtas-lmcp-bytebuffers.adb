with GNAT.Byte_Swapping;

package body avtas.lmcp.byteBuffers is

--     function To_Boolean(this : ByteBuffer) return Boolean is
--
--     function To_Int16(this : ByteBuffer) return Int16_t is
--
--     function To_Int32(this : ByteBuffer) return Int32_t is
--
--     function To_Int64(this : ByteBuffer) return Int64_t is
--
--     function To_UInt16(this : ByteBuffer) return UInt16_t is
--
--     function To_UInt32(this : ByteBuffer) return UInt32_t is
--
--     function To_Float(this : ByteBuffer) return Float_t is
--
--     function To_Double(this : ByteBuffer) return Double_t is
--
--     function To_String(this : ByteBuffer; numBytes : UInt32_t) return Unbounded_String is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is Unbounded_String;
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--     begin
--        return convert(this.Buf(this.Pos .. this.Pos + numBytes - 1));
--     end To_String;

   procedure Get_Boolean(this : in out ByteBuffer; output : out Boolean) is
   begin
      output := (if this.Buf(this.Pos) = 0 then False else True);
      this.Pos := this.Pos + 1;
   end Get_Boolean;

   procedure Get_Int16(this : in out ByteBuffer; output : out Int16_t) is
      subtype sourceType is ByteArray2;
      subtype swapType is ByteArray2;
      subtype targetType is Int16_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped2 (swapType);
   begin
      output := Convert(SwapBytes(this.Buf(this.Pos .. this.Pos + 1)));
      this.Pos := this.Pos + 2;
   end Get_Int16;

   procedure Get_Int32(this : in out ByteBuffer; output : out Int32_t) is
      subtype sourceType is ByteArray4;
      subtype swapType is ByteArray4;
      subtype targetType is Int32_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
   begin
      output := Convert(this.Buf(this.Pos .. this.Pos + 3));
      this.Pos := this.Pos + 4;
   end Get_Int32;

   procedure Get_Int64(this : in out ByteBuffer; output : out Int64_t) is
      subtype sourceType is ByteArray8;
      subtype swapType is ByteArray8;
      subtype targetType is Int64_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped8 (swapType);
   begin
      output := Convert(this.Buf(this.Pos .. this.Pos + 7));
      this.Pos := this.Pos + 8;
   end Get_Int64;

   procedure Get_UInt16(this : in out ByteBuffer; output : out UInt16_t) is
      subtype sourceType is ByteArray2;
      subtype swapType is ByteArray2;
      subtype targetType is UInt16_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped2 (swapType);
   begin
      output := Convert(this.Buf(this.Pos .. this.Pos + 1));
      this.Pos := this.Pos + 2;
   end Get_UInt16;

   procedure Get_UInt32(this : in out ByteBuffer; output : out UInt32_t) is
      subtype sourceType is ByteArray4;
      subtype swapType is ByteArray4;
      subtype targetType is UInt32_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
   begin
      output := Convert(this.Buf(this.Pos .. this.Pos + 3));
      this.Pos := this.Pos + 4;
   end Get_UInt32;

   procedure Get_Float(this : in out ByteBuffer; output : out Float_t) is
      subtype sourceType is ByteArray4;
      subtype swapType is ByteArray4;
      subtype targetType is Float_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
   begin
      output := Convert(this.Buf(this.Pos .. this.Pos + 3));
      this.Pos := this.Pos + 4;
   end Get_Float;

   procedure Get_Double(this : in out ByteBuffer; output : out Double_t) is
      subtype sourceType is ByteArray8;
      subtype swapType is ByteArray8;
      subtype targetType is Double_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped8 (swapType);
   begin
      output := Convert(this.Buf(this.Pos .. this.Pos + 7));
      this.Pos := this.Pos + 8;
   end Get_Double;

   procedure Get_String(this : in out ByteBuffer; output : out Unbounded_String) is
      numBytes : UInt16_t;
   begin
      Get_UInt16(this, numBytes);
      declare
         subtype sourceType is ByteArray(1 .. UInt32_t(numBytes));
         subtype targetType is Unbounded_String;
         function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      begin
         output := convert(this.Buf(this.Pos .. this.Pos + UInt32_t(numBytes) - 1));
         this.Pos := this.Pos + UInt32_t(numBytes);
      end;
   end Get_String;

   procedure Put_Boolean(this : in out ByteBuffer; input : in Boolean) is
   begin
      this.Buf(this.Pos) := (if input = False then 0 else 1);
      this.Pos := this.Pos + 1;
   end Put_Boolean;

   procedure Put_Int16(this :  in out ByteBuffer; input : in Int16_t) is
      subtype sourceType is Int16_t;
      subtype targetType is ByteArray2;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      subtype swapType is ByteArray2;
      function SwapBytes is new GNAT.Byte_Swapping.Swapped2 (swapType);
   begin
      this.Buf(this.Pos .. this.Pos + 1) := SwapBytes(Convert(input));
      this.Pos := this.Pos + 2;
   end Put_Int16;

   procedure Put_Int32(this :  in out ByteBuffer; input : in Int32_t) is
      subtype sourceType is Int32_t;
      subtype targetType is ByteArray4;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      subtype swapType is ByteArray4;
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
   begin
      this.Buf(this.Pos .. this.Pos + 3) := SwapBytes(Convert(input));
      this.Pos := this.Pos + 4;
   end Put_Int32;

   procedure Put_Int64(this :  in out ByteBuffer; input : in Int64_t) is
      subtype sourceType is Int64_t;
      subtype targetType is ByteArray8;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      subtype swapType is ByteArray8;
      function SwapBytes is new GNAT.Byte_Swapping.Swapped8 (swapType);
   begin
      this.Buf(this.Pos .. this.Pos + 7) := SwapBytes(Convert(input));
      this.Pos := this.Pos + 8;
   end Put_Int64;

   procedure Put_UInt16(this :  in out ByteBuffer; input : in UInt16_t) is
      subtype sourceType is UInt16_t;
      subtype targetType is ByteArray2;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      subtype swapType is ByteArray2;
      function SwapBytes is new GNAT.Byte_Swapping.Swapped2 (swapType);
   begin
      this.Buf(this.Pos .. this.Pos + 1) := SwapBytes(Convert(input));
      this.Pos := this.Pos + 2;
   end Put_UInt16;

   procedure Put_UInt32(this :  in out ByteBuffer; input : in UInt32_t) is
      subtype sourceType is UInt32_t;
      subtype targetType is ByteArray4;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      subtype swapType is ByteArray4;
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
   begin
      this.Buf(this.Pos .. this.Pos + 3) := SwapBytes(Convert(input));
      this.Pos := this.Pos + 4;
   end Put_UInt32;

   procedure Put_Float(this :  in out ByteBuffer; input : in Float_t) is
      subtype sourceType is Float_t;
      subtype targetType is ByteArray4;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      subtype swapType is ByteArray4;
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
   begin
      this.Buf(this.Pos .. this.Pos + 3) := SwapBytes(Convert(input));
      this.Pos := this.Pos + 4;
   end Put_Float;

   procedure Put_Double(this :  in out ByteBuffer; input : in Double_t) is
      subtype sourceType is Double_t;
      subtype targetType is ByteArray8;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      subtype swapType is ByteArray8;
      function SwapBytes is new GNAT.Byte_Swapping.Swapped8 (swapType);
   begin
      this.Buf(this.Pos .. this.Pos + 7) := SwapBytes(Convert(input));
      this.Pos := this.Pos + 8;
   end Put_Double;

   procedure Put_String(this :  in out ByteBuffer; input : in Unbounded_String) is
      subtype sourceType is Unbounded_String;
      subtype targetType is ByteArray(1 .. UInt32_t(Length(input)));
      function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
   begin
      Put_UInt16(this, UInt16_t(Length(input)));
      this.Buf(this.Pos .. this.Pos + UInt32_t(Length(input))) := Convert(input);
      this.Pos := this.Pos + UInt32_t(Length(input));
   end Put_String;

--     function To_BooleanArray(this : ByteBuffer; numBytes : UInt32_t) return BooleanArray is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is BooleanArray(1 .. numBytes);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--     begin
--        return Convert(this.Buf(this.Pos .. this.Pos + numBytes - 1));
--     end To_BooleanArray;
--
--     function To_Int16Array (this : ByteBuffer; numBytes : UInt32_t) return Int16Array is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is Int16Array(1 .. numBytes/2);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 2);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped2 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/2 loop
--           tempArray(Nat(2*i - 1) .. Nat(2*i)) := SwapBytes(tempArray(Nat(2*i - 1) .. Nat(2*i)));
--        end loop;
--        return convert(tempArray);
--     end To_Int16Array;
--
--     function To_Int32Array (this : ByteBuffer; numBytes : UInt32_t) return Int32Array is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is Int32Array(1 .. numBytes/4);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 4);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/4 loop
--           tempArray(Nat(4*i - 3) .. Nat(4*i)) := SwapBytes(tempArray(Nat(4*i - 3) .. Nat(4*i)));
--        end loop;
--        return convert(tempArray);
--     end To_Int32Array;
--
--     function To_Int64Array (this : ByteBuffer; numBytes : UInt32_t) return Int64Array is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is Int64Array(1 .. numBytes/8);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 8);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped8 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/8 loop
--           tempArray(Nat(8*i - 7) .. Nat(8*i)) := SwapBytes(tempArray(Nat(8*i - 7) .. Nat(8*i)));
--        end loop;
--        return convert(tempArray);
--     end To_Int64Array;
--
--     function To_UInt16Array (this : ByteBuffer; numBytes : UInt32_t) return UInt16Array is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is UInt16Array(1 .. numBytes/2);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 2);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped2 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/2 loop
--           tempArray(Nat(2*i - 1) .. Nat(2*i)) := SwapBytes(tempArray(Nat(2*i - 1) .. Nat(2*i)));
--        end loop;
--        return convert(tempArray);
--     end To_UInt16Array;
--
--     function To_UInt32Array (this : ByteBuffer; numBytes : UInt32_t) return UInt32Array is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is UInt32Array(1 .. numBytes/4);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 4);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/4 loop
--           tempArray(Nat(4*i - 3) .. Nat(4*i)) := SwapBytes(tempArray(Nat(4*i - 3) .. Nat(4*i)));
--        end loop;
--        return convert(tempArray);
--     end To_UInt32Array;
--
--     function To_FloatArray(this : ByteBuffer; numBytes : UInt32_t) return FloatArray is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is FloatArray(1 .. numBytes/4);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 4);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/4 loop
--           tempArray(Nat(4*i - 3) .. Nat(4*i)) := SwapBytes(tempArray(Nat(4*i - 3) .. Nat(4*i)));
--        end loop;
--        return convert(tempArray);
--     end To_FloatArray;
--
--     function To_DoubleArray(this : ByteBuffer; numBytes : UInt32_t) return DoubleArray is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is DoubleArray(1 .. numBytes/8);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 8);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped8 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/8 loop
--           tempArray(Nat(8*i - 7) .. Nat(8*i)) := SwapBytes(tempArray(Nat(8*i - 7) .. Nat(8*i)));
--        end loop;
--        return convert(tempArray);
--     end To_DoubleArray;
--
--     function To_StringArray(Input : ByteArray) return StringArray is
--        arrayElements : UInt16_t := To_UInt16(ByteArray2(Input(1 .. 2)));
--        position : UInt32_t := 3;
--        stringLength : UInt16_t;
--        tempString : Unbounded_String;
--        output : StringArray(Nat(1) .. Nat(arrayElements));
--        function convert is new Ada.Unchecked_Conversion(Source => Byte, Target => Character);
--     begin
--        Put(arrayElements'Image);
--        New_Line;
--        for i in 1 .. arrayElements loop
--           stringLength := To_UInt16(ByteArray2(Input(Nat(position) .. Nat(position + 1))));
--           Put(stringLength'Image);
--           New_Line;
--           tempString := To_Unbounded_String("");
--           position := position + 2;
--           for j in 1 .. stringLength loop
--              tempString := tempString & convert(Input(Nat(position) + Nat(j) - Nat(1)));
--           end loop;
--           position := position + UInt32_t(stringLength);
--           output(Nat(i)) := tempString;
--        end loop;
--        return output;
--     end To_StringArray;

--     function To_BooleanArray (this : in out ByteBuffer; output : out BooleanArray; isLarge : in Boolean) return BooleanArray is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is BooleanArray(1 .. numBytes);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--     begin
--        return Convert(this.Buf(this.Pos .. this.Pos + numBytes - 1));
--     end To_BooleanArray;
--  
--     function To_Int16Array (this : ByteBuffer; numBytes : UInt32_t) return Int16Array is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is Int16Array(1 .. numBytes/2);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 2);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped2 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/2 loop
--           tempArray(Nat(2*i - 1) .. Nat(2*i)) := SwapBytes(tempArray(Nat(2*i - 1) .. Nat(2*i)));
--        end loop;
--        return convert(tempArray);
--     end To_Int16Array;
--  
--     function To_Int32Array (this : ByteBuffer; numBytes : UInt32_t) return Int32Array is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is Int32Array(1 .. numBytes/4);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 4);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/4 loop
--           tempArray(Nat(4*i - 3) .. Nat(4*i)) := SwapBytes(tempArray(Nat(4*i - 3) .. Nat(4*i)));
--        end loop;
--        return convert(tempArray);
--     end To_Int32Array;
--  
--     function To_Int64Array (this : ByteBuffer; numBytes : UInt32_t) return Int64Array is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is Int64Array(1 .. numBytes/8);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 8);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped8 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/8 loop
--           tempArray(Nat(8*i - 7) .. Nat(8*i)) := SwapBytes(tempArray(Nat(8*i - 7) .. Nat(8*i)));
--        end loop;
--        return convert(tempArray);
--     end To_Int64Array;
--  
--     function To_UInt16Array (this : ByteBuffer; numBytes : UInt32_t) return UInt16Array is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is UInt16Array(1 .. numBytes/2);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 2);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped2 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/2 loop
--           tempArray(Nat(2*i - 1) .. Nat(2*i)) := SwapBytes(tempArray(Nat(2*i - 1) .. Nat(2*i)));
--        end loop;
--        return convert(tempArray);
--     end To_UInt16Array;
--  
--     function To_UInt32Array (this : ByteBuffer; numBytes : UInt32_t) return UInt32Array is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is UInt32Array(1 .. numBytes/4);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 4);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/4 loop
--           tempArray(Nat(4*i - 3) .. Nat(4*i)) := SwapBytes(tempArray(Nat(4*i - 3) .. Nat(4*i)));
--        end loop;
--        return convert(tempArray);
--     end To_UInt32Array;
--  
--     function To_FloatArray(this : ByteBuffer; numBytes : UInt32_t) return FloatArray is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is FloatArray(1 .. numBytes/4);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 4);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/4 loop
--           tempArray(Nat(4*i - 3) .. Nat(4*i)) := SwapBytes(tempArray(Nat(4*i - 3) .. Nat(4*i)));
--        end loop;
--        return convert(tempArray);
--     end To_FloatArray;
--  
--     function To_DoubleArray(this : ByteBuffer; numBytes : UInt32_t) return DoubleArray is
--        subtype sourceType is ByteArray(1 .. numBytes);
--        subtype targetType is DoubleArray(1 .. numBytes/8);
--        function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
--        subtype swapType is ByteArray(1 .. 8);
--        function SwapBytes is new GNAT.Byte_Swapping.Swapped8 (swapType);
--        tempArray : ByteArray := this.Buf(this.Pos .. this.Pos + numBytes - 1);
--     begin
--        for i in 1 .. tempArray'Length/8 loop
--           tempArray(Nat(8*i - 7) .. Nat(8*i)) := SwapBytes(tempArray(Nat(8*i - 7) .. Nat(8*i)));
--        end loop;
--        return convert(tempArray);
--     end To_DoubleArray;
--  
--     function To_StringArray(Input : ByteArray) return StringArray is
--        arrayElements : UInt16_t := To_UInt16(ByteArray2(Input(1 .. 2)));
--        position : UInt32_t := 3;
--        stringLength : UInt16_t;
--        tempString : Unbounded_String;
--        output : StringArray(Nat(1) .. Nat(arrayElements));
--        function convert is new Ada.Unchecked_Conversion(Source => Byte, Target => Character);
--     begin
--        Put(arrayElements'Image);
--        New_Line;
--        for i in 1 .. arrayElements loop
--           stringLength := To_UInt16(ByteArray2(Input(Nat(position) .. Nat(position + 1))));
--           Put(stringLength'Image);
--           New_Line;
--           tempString := To_Unbounded_String("");
--           position := position + 2;
--           for j in 1 .. stringLength loop
--              tempString := tempString & convert(Input(Nat(position) + Nat(j) - Nat(1)));
--           end loop;
--           position := position + UInt32_t(stringLength);
--           output(Nat(i)) := tempString;
--        end loop;
--        return output;
--     end To_StringArray;

   procedure setPosition(this : in out ByteBuffer; position : in Nat) is
   begin
      this.Pos := position;
   end setPosition;

   procedure incrementPosition(this : in out ByteBuffer; amount : in UInt32_t) is
   begin
      if(amount > 0) then
         this.Pos := this.Pos + Nat(amount);
      end if;
   end;

end avtas.lmcp.byteBuffers;
