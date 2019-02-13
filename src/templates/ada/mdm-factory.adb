-<include_all_series_headers>-
with Ada.Unchecked_Conversion;
with GNAT.Byte_Swapping;

package body -<full_series_name_dots>-.factory is

   procedure PutObject (Object : in Avtas.Lmcp.Object.Object_Any; Buffer : in out ByteBuffer);
   

   function PackMessage (RootObject : in Avtas.Lmcp.Object.Object_Any; EnableChecksum : in Boolean) return ByteBuffer is
      -- Allocate space for message, with 15 extra bytes for
      --  Existence (1 byte), series name (8 bytes), type (4 bytes), version number (2 bytes)
      MsgSize : constant UInt32 := RootObject.CalculatePackedSize + 15;
      Buffer  : ByteBuffer (HEADER_SIZE + MsgSize + CHECKSUM_SIZE);
   begin
      -- add header values
      Buffer.Put_Int32 (LMCP_CONTROL_STR);
      Buffer.Put_UInt32 (MsgSize);
      
      -- add root object
      PutObject (RootObject, Buffer);
      
      -- add checksum if enabled
      Buffer.Put_UInt32 ((if EnableChecksum then CalculateChecksum (Buffer) else 0));
      return Buffer;
   end PackMessage;
   
   procedure PutObject (Object : in Avtas.Lmcp.Object.Object_Any; Buffer : in out ByteBuffer) is
   begin
      -- If object is null, pack a 0; otherwise, add root object
      if Object = null then
         Buffer.Put_Boolean (False);
      else
         Buffer.Put_Boolean (True);
         Buffer.Put_Int64 (Object.GetSeriesNameAsLong);
         Buffer.Put_UInt32 (Object.GetLmcpType);
         Buffer.Put_UInt16 (Object.GetSeriesVersion);
         Pack (Object, Buffer);
      end if;
   end PutObject;

   procedure GetObject (Buffer : in out ByteBuffer; Output : out Avtas.Lmcp.Object.Object_Any) is
      CtrlStr   : Int32;
      MsgSize   : UInt32;
      MsgExists : Boolean;
      SeriesId  : Int64;
      MsgType   : Uint32;
      Version   : Uint16;
   begin
      Output := null; -- default
      -- TODO: add some kind of warning/error messages for each null case
      if buffer.Capacity < HEADER_SIZE + CHECKSUM_SIZE then
         return;
      end if;
      Buffer.Get_Int32 (CtrlStr);
      if CtrlStr /= LMCP_CONTROL_STR then
         return;
      end if;
      Buffer.Get_UInt32 (MsgSize);
      if Buffer.Capacity < MsgSize then
         return;
      end if;
      if not validate (buffer) then
         return;
      end if;
      Buffer.Get_Boolean (MsgExists);
      if not MsgExists then
         return;
      end if;
      
      Buffer.Get_Int64 (SeriesId);
      Buffer.Get_UInt32 (MsgType);
      Buffer.Get_UInt16 (Version);
      Output := CreateObject (SeriesId, MsgType, Version);
      if Output /= null then
         Unpack (Output, Buffer);
      end if;
   end GetObject;

   function createObject(seriesId : in Int64; msgType : in UInt32; version: in UInt16) return avtas.lmcp.object.Object_Any is
   begin
      -<series_factory_switch>-
   end createObject;

   function calculateChecksum (Buffer : in ByteBuffer) return UInt32 is
      Sum   : UInt32 := 0;
      Bytes : constant Byte_Array := Buffer.Raw_Bytes;  -- TODO: this is making a copy, 1 .. Position - 1
   begin
      for K in Index range 1 .. Bytes'Length - Checksum_Size loop
         Sum := Sum + UInt32 (Bytes (K));
      end loop;
      return sum;
   end calculateChecksum;

   subtype Four_Bytes is Byte_Array (1 .. 4);  -- for arbitrary UInt32 values as well as for checksums
   
   pragma Compile_Time_Error (UInt32'Object_Size /= 4 * 8, "UInt32 expected to be 4 bytes");
   
   pragma Compile_Time_Error (Checksum_Size /= 4, "Checksum_Size expected to be 4 bytes");
   
   function As_UInt32 is new Ada.Unchecked_Conversion (Source => Four_Bytes, Target => UInt32);
   
   function Swapped4 is new GNAT.Byte_Swapping.Swapped4 (Four_Bytes);   
   
   function GetObjectSize (Buffer : in ByteBuffer) return UInt32 is
      Msg_Size_Bytes : constant Four_Bytes := Buffer.Raw_Bytes (First => 5, Last => 8);  -- the second UInt32 in the buffer
   begin
      return As_UInt32 (Swapped4 (Msg_Size_Bytes));
   end getObjectSize;

   function validate (Buffer : in ByteBuffer) return Boolean is
      Sum   : UInt32;      
      Checksum_Bytes : constant Four_Bytes := Buffer.Tail (Length => Checksum_Size);  -- the last 4 bytes in the buffer
   begin
      sum := calculateChecksum (buffer);
      return Sum = 0 or else 
             Sum = As_UInt32 (Swapped4 (Checksum_Bytes));
   end validate;

end -<full_series_name_dots>-.factory;
