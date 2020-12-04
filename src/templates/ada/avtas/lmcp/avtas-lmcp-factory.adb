-<include_all_factories>-
with Ada.Unchecked_Conversion;

package body AVTAS.LMCP.Factory is

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
      Buffer.Put_UInt32 (if EnableChecksum then CalculateChecksum (Buffer, Buffer.Length) else 0);
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
         Object.Pack (Buffer);
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
         Output.Unpack (Buffer);
      end if;
   end GetObject;

   function createObject(seriesId : in Int64; msgType : in UInt32; version: in UInt16) return avtas.lmcp.object.Object_Any is
   begin
      -<global_factory_switch>-
   end createObject;

  function CalculateChecksum (Buffer : in ByteBuffer; Last : in Index) return UInt32 is
     (Buffer.Checksum (From => 1, To => Last));

   function GetObjectSize (Buffer : in ByteBuffer) return UInt32 is
      Result : UInt32;
   begin
      Buffer.Get_UInt32 (Result, First => 5);  -- the second UInt32 value in the buffer
      return Result;
   end getObjectSize;

   function Validate (Buffer : in ByteBuffer) return Boolean is
      Computed_Checksum : UInt32;
      Existing_Checksum : UInt32;
   begin
      Computed_Checksum := CalculateChecksum (Buffer, Last => Buffer.Length - Checksum_Size);  -- all except the existing checksum
      if Computed_Checksum = 0 then
         return True;
      else
         Buffer.Get_UInt32 (Existing_Checksum, First => Buffer.Length - Checksum_Size + 1);  -- the existing checksum
         return Computed_Checksum = Existing_Checksum;
      end if;
   end Validate;

end AVTAS.LMCP.Factory;

