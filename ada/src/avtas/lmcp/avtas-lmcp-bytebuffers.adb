with GNAT.Byte_Swapping;

package body avtas.lmcp.byteBuffers is

   function To_Int16(Input : ByteArray2) return Int16_t is
      subtype sourceType is ByteArray2;
      subtype swapType is ByteArray2;
      subtype targetType is Int16_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped2 (swapType);
   begin
      return Convert(SwapBytes(Input));
   end To_Int16;

   function To_Int32(Input : ByteArray4) return Int32_t is
      subtype sourceType is ByteArray4;
      subtype swapType is ByteArray4;
      subtype targetType is Int32_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
   begin
      return Convert(SwapBytes(Input));
   end To_Int32;

   function To_Int64(Input : ByteArray8) return Int64_t is
      subtype sourceType is ByteArray8;
      subtype swapType is ByteArray8;
      subtype targetType is Int64_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped8 (swapType);
   begin
      return Convert(SwapBytes(Input));
   end To_Int64;

   function To_UInt16(Input : ByteArray2) return UInt16_t is
      subtype sourceType is ByteArray2;
      subtype swapType is ByteArray2;
      subtype targetType is UInt16_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped2 (swapType);
   begin
      return Convert(SwapBytes(Input));
   end To_UInt16;

   function To_UInt32(Input : ByteArray4) return UInt32_t is
      subtype sourceType is ByteArray4;
      subtype swapType is ByteArray4;
      subtype targetType is UInt32_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
   begin
      return Convert(SwapBytes(Input));
   end To_UInt32;

   function To_Float(Input : ByteArray4) return Float_t is
      subtype sourceType is ByteArray4;
      subtype swapType is ByteArray4;
      subtype targetType is Float_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
   begin
      return Convert(SwapBytes(Input));
   end To_Float;

   function To_Double(Input : ByteArray8) return Double_t is
      subtype sourceType is ByteArray8;
      subtype swapType is ByteArray8;
      subtype targetType is Double_t;
      function Convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped8 (swapType);
   begin
      return Convert(SwapBytes(Input));
   end To_Double;

   function To_BooleanArray(Input : ByteArray) return BooleanArray is
      subtype sourceType is ByteArray(Input'Range);
      subtype targetType is BooleanArray(Input'Range);
      function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
   begin
      return convert(Input);
   end To_BooleanArray;

   function To_Int16Array (Input : ByteArray) return Int16Array is
      subtype sourceType is ByteArray(Input'Range);
      subtype swapType is ByteArray(1 .. 2);
      subtype targetType is Int16Array(1 .. Input'Length/2);
      function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped2 (swapType);
      tempArray : ByteArray := Input;
   begin
      for i in 1 .. tempArray'Length/2 loop
         tempArray(Nat(2*i - 1) .. Nat(2*i)) := SwapBytes(tempArray(Nat(2*i - 1) .. Nat(2*i)));
      end loop;
      return convert(tempArray);
   end To_Int16Array;

   function To_Int32Array (Input : ByteArray) return Int32Array is
      subtype sourceType is ByteArray(Input'Range);
      subtype swapType is ByteArray(1 .. 4);
      subtype targetType is Int32Array(1 .. Input'Length/4);
      function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
      tempArray : ByteArray := Input;
   begin
      for i in 1 .. tempArray'Length/4 loop
         tempArray(Nat(4*i - 3) .. Nat(4*i)) := SwapBytes(tempArray(Nat(4*i - 3) .. Nat(4*i)));
      end loop;
      return convert(tempArray);
   end To_Int32Array;

   function To_Int64Array (Input : ByteArray) return Int64Array is
      subtype sourceType is ByteArray(Input'Range);
      subtype swapType is ByteArray(1 .. 8);
      subtype targetType is Int64Array(1 .. Input'Length/8);
      function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped8 (swapType);
      tempArray : ByteArray := Input;
   begin
      for i in 1 .. tempArray'Length/8 loop
         tempArray(Nat(8*i - 7) .. Nat(8*i)) := SwapBytes(tempArray(Nat(8*i - 7) .. Nat(8*i)));
      end loop;
      return convert(tempArray);
   end To_Int64Array;

   function To_UInt16Array (Input : ByteArray) return UInt16Array is
      subtype sourceType is ByteArray(Input'Range);
      subtype swapType is ByteArray(1 .. 2);
      subtype targetType is UInt16Array(1 .. Input'Length/2);
      function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped2 (swapType);
      tempArray : ByteArray := Input;
   begin
      for i in 1 .. tempArray'Length/2 loop
         tempArray(Nat(2*i - 1) .. Nat(2*i)) := SwapBytes(tempArray(Nat(2*i - 1) .. Nat(2*i)));
      end loop;
      return convert(tempArray);
   end To_UInt16Array;

   function To_UInt32Array (Input : ByteArray) return UInt32Array is
      subtype sourceType is ByteArray(Input'Range);
      subtype swapType is ByteArray(1 .. 4);
      subtype targetType is UInt32Array(1 .. Input'Length/4);
      function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
      tempArray : ByteArray := Input;
   begin
      for i in 1 .. tempArray'Length/4 loop
         tempArray(Nat(4*i - 3) .. Nat(4*i)) := SwapBytes(tempArray(Nat(4*i - 3) .. Nat(4*i)));
      end loop;
      return convert(tempArray);
   end To_UInt32Array;

   function To_FloatArray(Input : ByteArray) return FloatArray is
      subtype sourceType is ByteArray(Input'Range);
      subtype swapType is ByteArray(1 .. 4);
      subtype targetType is FloatArray(1 .. Input'Length/4);
      function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
      tempArray : ByteArray := Input;
   begin
      for i in 1 .. tempArray'Length/4 loop
         tempArray(Nat(4*i - 3) .. Nat(4*i)) := SwapBytes(tempArray(Nat(4*i - 3) .. Nat(4*i)));
      end loop;
      return convert(tempArray);
   end To_FloatArray;

   function To_DoubleArray(Input : ByteArray) return DoubleArray is
      subtype sourceType is ByteArray(Input'Range);
      subtype swapType is ByteArray(1 .. 8);
      subtype targetType is DoubleArray(1 .. Input'Length/8);
      function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
      function SwapBytes is new GNAT.Byte_Swapping.Swapped8 (swapType);
      tempArray : ByteArray := Input;
   begin
      for i in 1 .. tempArray'Length/8 loop
         tempArray(Nat(8*i - 7) .. Nat(8*i)) := SwapBytes(tempArray(Nat(8*i - 7) .. Nat(8*i)));
      end loop;
      return convert(tempArray);
   end To_DoubleArray;

   function To_String(Input : ByteArray) return Unbounded_String is
      subtype sourceType is ByteArray(Input'Range);
      subtype targetType is Unbounded_String;
      function convert is new Ada.Unchecked_Conversion(Source => sourceType, Target => targetType);
   begin
      return convert(Input);
   end To_String;

   function To_StringArray(Input : ByteArray) return StringArray is
      arrayElements : UInt16_t := To_UInt16(ByteArray2(Input(1 .. 2)));
      position : UInt32_t := 3;
      stringLength : UInt16_t;
      tempString : Unbounded_String;
      output : StringArray(Nat(1) .. Nat(arrayElements));
      function convert is new Ada.Unchecked_Conversion(Source => Byte, Target => Character);
   begin
      Put(arrayElements'Image);
      New_Line;
      for i in 1 .. arrayElements loop
         stringLength := To_UInt16(ByteArray2(Input(Nat(position) .. Nat(position + 1))));
         Put(stringLength'Image);
         New_Line;
         tempString := To_Unbounded_String("");
         position := position + 2;
         for j in 1 .. stringLength loop
            tempString := tempString & convert(Input(Nat(position) + Nat(j) - Nat(1)));
         end loop;
         position := position + UInt32_t(stringLength);
         output(Nat(i)) := tempString;
      end loop;
      return output;
   end To_StringArray;

end avtas.lmcp.byteBuffers;
