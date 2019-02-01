-<include_all_factories>-
with Ada.Unchecked_Conversion;
with GNAT.Byte_Swapping;

package body avtas.lmcp.factory is

   function packMessage(rootObject : in avtas.lmcp.object.Object_Any; enableChecksum : in Boolean) return ByteBuffer is
      msgSize : UInt32;
   begin
      -- Allocate space for message, with 15 extra bytes for
      --  Existence (1 byte), series name (8 bytes), type (4 bytes), version number (2 bytes)
      msgSize := rootObject.calculatePackedSize + 15;
      declare
         buffer : ByteBuffer(HEADER_SIZE + msgSize + CHECKSUM_SIZE);
      begin
         -- add header values
         Put_Int32(LMCP_CONTROL_STR, buffer);
         Put_UInt32(msgSize, buffer);

         -- add root object
         putObject(rootObject, buffer);
           
         -- add checksum if enabled
         Put_UInt32((if enableChecksum then calculateChecksum(buffer) else 0), buffer);
         return buffer;
      end;
   end packMessage;
   
   procedure putObject(object : in avtas.lmcp.object.Object_Any; buffer : in out ByteBuffer) is
   begin
         -- If object is null, pack a 0; otherwise, add root object
         if(object = null) then
            Put_Boolean(False, buffer);
         else
            Put_Boolean(True, buffer);
            Put_Int64(object.getSeriesNameAsLong, buffer);
            Put_UInt32(object.getLmcpType, buffer);
            Put_UInt16(object.getSeriesVersion, buffer);
            pack(object, buffer);
         end if;
   end putObject;

   procedure getObject(buffer : in out ByteBuffer; output : out avtas.lmcp.object.Object_Any) is
      ctrlStr : Int32;
      msgSize : UInt32;
      msgExists : Boolean;
      seriesId : Int64;
      msgType : Uint32;
      version : Uint16;
   begin
      -- TODO: add some kind of warning/error messages for each null case
      if buffer.Capacity < HEADER_SIZE + CHECKSUM_SIZE then
         output := null;
      else
         Get_Int32(buffer, ctrlStr);
         if ctrlStr /= LMCP_CONTROL_STR then
            output := null;
         else
            Get_UInt32(buffer, msgSize);
            if buffer.Capacity < msgSize then
               output := null;
            elsif(validate(buffer) = False) then
               output := null;
            else
               Get_Boolean(buffer, msgExists);
               if(msgExists = False) then
                  output := null;
               else
                  Get_Int64(buffer, seriesId);
                  Get_UInt32(buffer, msgType);
                  Get_UInt16(buffer, version);
                  output := createObject(seriesId, msgType, version);
                  if (output /= null) then
                     unpack(buffer, output);
                  end if;
               end if;
            end if;
         end if;
      end if;
   end getObject;

   function createObject(seriesId : in Int64; msgType : in UInt32; version: in UInt16) return avtas.lmcp.object.Object_Any is
   begin
      case seriesId is
         when 4849604199710720000 => return afrl.cmasi.factory.createObject(seriesId, msgType, version);
         when others => return null;
      end case;
   end createObject;

   function calculateChecksum (buffer : in ByteBuffer) return UInt32 is
      sum : UInt32 := 0;
      function IntToByteArray is new Ada.Unchecked_Conversion(Source => UInt32, Target => ByteArray4);
      function ByteArrayToInt is new Ada.Unchecked_Conversion(Source => ByteArray4, Target => UInt32);
   begin
      for i in 1 .. buffer.Capacity - CHECKSUM_SIZE loop
         sum := sum + UInt32(buffer.Buf(i));
      end loop;
      -- The C++ code does the following, but why? It seems like a no-op to me
      -- Can't we just return sum?
      -- return (ByteArrayToInt(IntToByteArray(sum) & IntToByteArray(UInt32(16#FFFFFFFF#))));
      return sum;
   end calculateChecksum;

   function getObjectSize (buffer : in ByteBuffer) return UInt32 is
      function ByteArrayToInt is new Ada.Unchecked_Conversion(Source => ByteArray4, Target => UInt32);
   begin
      return ByteArrayToInt(buffer.Buf(5 .. 8));
   end getObjectSize;

   function validate(buffer : in ByteBuffer) return Boolean is
      function ByteArrayToInt is new Ada.Unchecked_Conversion(Source => ByteArray4, Target => UInt32);
      subtype SwapType is ByteArray4;
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
      sum : UInt32;
   begin
      sum := calculateChecksum(buffer);
      return sum = 0 or else sum = ByteArrayToInt(SwapBytes(buffer.Buf(buffer.Buf'Last - 3 .. buffer.Buf'Last)));
   end validate;

end avtas.lmcp.factory;
