-<include_all_series_headers>-
with Ada.Unchecked_Conversion;
with Ada.Text_IO;  use Ada.Text_IO;

package body -<full_series_name_dots>-.Factory is

   procedure PutObject (Object : in Avtas.Lmcp.Object.Object_Any; Buffer : in out ByteBuffer);
   
   -----------------
   -- PackMessage --
   -----------------

   function PackMessage (RootObject : in Avtas.Lmcp.Object.Object_Any; EnableChecksum : in Boolean) return ByteBuffer is
      --  Allocate space for message, with 15 extra bytes for
      --  Existence (1 byte), series name (8 bytes), type (4 bytes), version number (2 bytes)
      MsgSize : constant UInt32 := RootObject.CalculatePackedSize + 15;
      Buffer  : ByteBuffer (HEADER_SIZE + Index (MsgSize) + CHECKSUM_SIZE);
   begin
      -- add header values
      Put_Int32 (Buffer, LMCP_CONTROL_STR);
      Put_UInt32 (Buffer, MsgSize);
      -- add root object
      PutObject (RootObject, Buffer);
      -- add checksum if enabled
      Put_UInt32 (Buffer, (if EnableChecksum then CalculatedChecksum (Buffer, High_Water_Mark (Buffer) - 1) else 0));
      return Buffer;
   end PackMessage;

   ---------------
   -- PutObject --
   ---------------

   procedure PutObject (Object : in Avtas.Lmcp.Object.Object_Any; Buffer : in out ByteBuffer) is
   begin
      -- If object is null, pack a 0; otherwise, add root object
      if Object = null then
         Put_Boolean (Buffer, False);
      else
         Put_Boolean (Buffer, True);
         Put_Int64 (Buffer, Object.GetSeriesNameAsLong);
         Put_UInt32 (Buffer, Object.GetLmcpType);
         Put_UInt16 (Buffer, Object.GetSeriesVersion);
         Object.Pack (Buffer);
      end if;
   end PutObject;

   ---------------
   -- GetObject --
   ---------------

   procedure GetObject (Buffer : in out ByteBuffer; Output : out Avtas.Lmcp.Object.Object_Any) is
      CtrlStr   : Int32;
      MsgSize   : UInt32;
      MsgExists : Boolean;
      SeriesId  : Int64;
      MsgType   : Uint32;
      Version   : Uint16;
   begin
      Output := null; -- default
      
      if buffer.Capacity < HEADER_SIZE + CHECKSUM_SIZE then
         Put_Line ("-<full_series_name_dots>-.Factory.GetObject error: Buffer capacity too small for header and checksum (" &
                   Integer'Image(HEADER_SIZE + CHECKSUM_SIZE) & ").");         
         raise Program_Error; -- for the moment
         --  return;
      end if;
      
      Get_Int32 (Buffer, CtrlStr);
      if CtrlStr /= LMCP_CONTROL_STR then
         Put_Line ("-<full_series_name_dots>-.Factory.GetObject error: Not a proper LMCP message.");
         Put_Line ("   Expected: " & LMCP_CONTROL_STR'Image & "   Received: " & CtrlStr'Image);
         raise Program_Error;  -- for the moment
         --  return;
      end if;
      
      Get_UInt32 (Buffer, MsgSize);
      if Buffer.Capacity < Index (MsgSize) then
         Put_Line ("-<full_series_name_dots>-.Factory.GetObject error: Buffer size too small for packed object.");
         Put_Line ("   MsgSize: " & MsgSize'Image & "    Capacity: " & Buffer.Capacity'Image);
         raise Program_Error;  -- for the moment
         --  return;
      end if;
      
      if not Validated (Buffer) then
         Put_Line ("-<full_series_name_dots>-.Factory.GetObject error: Checksum invalid.");
         raise Program_Error;  -- for the moment
         --  return;
      end if;
      
      Get_Boolean (Buffer, MsgExists);
      if not MsgExists then
         Put_Line ("-<full_series_name_dots>-.Factory.GetObject error: Message indicated it was packed as NULL");
         raise Program_Error;  -- for the moment
         --  return;
      end if;

      Get_Int64 (Buffer, SeriesId);
      Get_UInt32 (Buffer, MsgType);
      Get_UInt16 (Buffer, Version);
      Output := CreateObject (SeriesId, MsgType, Version);
      if Output /= null then
         Output.Unpack (Buffer);
      else
         Put_Line ("-<full_series_name_dots>-.Factory.GetObject error: Buffer does not contain a recognized object type.");
      end if;
   end GetObject;

   ------------------
   -- createObject --
   ------------------

   function createObject(seriesId : in Int64; msgType : in UInt32; version: in UInt16) return avtas.lmcp.object.Object_Any is
   begin
      -<series_factory_switch>-
   end createObject;

   ------------------------
   -- CalculatedChecksum --
   ------------------------

   function CalculatedChecksum (Buffer : in ByteBuffer; Size : Index) return UInt32 is
     (Checksum (Buffer, Last => Size));

   -------------------
   -- GetObjectSize --
   -------------------

   function GetObjectSize (Buffer : in ByteBuffer) return UInt32 is
      Result : UInt32;
      Second_HalfWord_Start : constant Index := 4;  -- the buffer array is zero-based
   begin
      --  get the second UInt32 value in the buffer
      Get_UInt32 (Buffer, Result, First => Second_HalfWord_Start);  
      return Result;
   end getObjectSize;

   ---------------
   -- Validated --
   ---------------

   function Validated (Buffer : in ByteBuffer) return Boolean is
      Stored_Checksum : UInt32;
   begin
      if High_Water_Mark (Buffer) < HEADER_SIZE + CHECKSUM_SIZE then
         Put_Line ("-<full_series_name_dots>-.Factory.Validated: Buffer size < HEADER_SIZE + CHECKSUM_SIZE.");
         return False;
      end if;
      Get_UInt32 (Buffer, Stored_Checksum, First => High_Water_Mark (Buffer) - Checksum_Size);
      return Stored_Checksum = 0 or else Stored_Checksum = CalculatedChecksum (Buffer, High_Water_Mark (Buffer) - Checksum_Size);
   end Validated;

end -<full_series_name_dots>-.Factory;

