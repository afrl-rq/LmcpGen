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
      if seriesId = 4849604199710720000 and then version = 3 then
         case msgType is
            when 1 => return new afrl.cmasi.AbstractGeometry.AbstractGeometry;
            when 2 => return new afrl.cmasi.KeyValuePair.KeyValuePair;
            when 3 => return new afrl.cmasi.Location3D.Location3D;
            when 4 => return new afrl.cmasi.PayloadAction.PayloadAction;
            when 5 => return new afrl.cmasi.PayloadConfiguration.PayloadConfiguration;
            when 6 => return new afrl.cmasi.PayloadState.PayloadState;
            when 7 => return new afrl.cmasi.VehicleAction.VehicleAction;
            when 8 => return new afrl.cmasi.LmcpTask.LmcpTask;
            when 9 => return new afrl.cmasi.SearchTask.SearchTask;
            when 10 => return new afrl.cmasi.AbstractZone.AbstractZone;
            when 11 => return new afrl.cmasi.EntityConfiguration.EntityConfiguration;
            when 12 => return new afrl.cmasi.FlightProfile.FlightProfile;
            when 13 => return new afrl.cmasi.AirVehicleConfiguration.AirVehicleConfiguration;
            when 14 => return new afrl.cmasi.EntityState.EntityState;
            when 15 => return new afrl.cmasi.AirVehicleState.AirVehicleState;
            when 16 => return new afrl.cmasi.Wedge.Wedge;
            when 17 => return new afrl.cmasi.AreaSearchTask.AreaSearchTask;
            when 18 => return new afrl.cmasi.CameraAction.CameraAction;
            when 19 => return new afrl.cmasi.CameraConfiguration.CameraConfiguration;
            when 20 => return new afrl.cmasi.GimballedPayloadState.GimballedPayloadState;
            when 21 => return new afrl.cmasi.CameraState.CameraState;
            when 22 => return new afrl.cmasi.Circle.Circle;
            when 23 => return new afrl.cmasi.GimbalAngleAction.GimbalAngleAction;
            when 24 => return new afrl.cmasi.GimbalConfiguration.GimbalConfiguration;
            when 25 => return new afrl.cmasi.GimbalScanAction.GimbalScanAction;
            when 26 => return new afrl.cmasi.GimbalStareAction.GimbalStareAction;
            when 27 => return new afrl.cmasi.GimbalState.GimbalState;
            when 28 => return new afrl.cmasi.GoToWaypointAction.GoToWaypointAction;
            when 29 => return new afrl.cmasi.KeepInZone.KeepInZone;
            when 30 => return new afrl.cmasi.KeepOutZone.KeepOutZone;
            when 31 => return new afrl.cmasi.LineSearchTask.LineSearchTask;
            when 32 => return new afrl.cmasi.NavigationAction.NavigationAction;
            when 33 => return new afrl.cmasi.LoiterAction.LoiterAction;
            when 34 => return new afrl.cmasi.LoiterTask.LoiterTask;
            when 35 => return new afrl.cmasi.Waypoint.Waypoint;
            when 36 => return new afrl.cmasi.MissionCommand.MissionCommand;
            when 37 => return new afrl.cmasi.MustFlyTask.MustFlyTask;
            when 38 => return new afrl.cmasi.OperatorSignal.OperatorSignal;
            when 39 => return new afrl.cmasi.OperatingRegion.OperatingRegion;
            when 40 => return new afrl.cmasi.AutomationRequest.AutomationRequest;
            when 41 => return new afrl.cmasi.PointSearchTask.PointSearchTask;
            when 42 => return new afrl.cmasi.Polygon.Polygon;
            when 43 => return new afrl.cmasi.Rectangle.Rectangle;
            when 44 => return new afrl.cmasi.RemoveTasks.RemoveTasks;
            when 45 => return new afrl.cmasi.ServiceStatus.ServiceStatus;
            when 46 => return new afrl.cmasi.SessionStatus.SessionStatus;
            when 47 => return new afrl.cmasi.VehicleActionCommand.VehicleActionCommand;
            when 48 => return new afrl.cmasi.VideoStreamAction.VideoStreamAction;
            when 49 => return new afrl.cmasi.VideoStreamConfiguration.VideoStreamConfiguration;
            when 50 => return new afrl.cmasi.VideoStreamState.VideoStreamState;
            when 51 => return new afrl.cmasi.AutomationResponse.AutomationResponse;
            when 52 => return new afrl.cmasi.RemoveZones.RemoveZones;
            when 53 => return new afrl.cmasi.RemoveEntities.RemoveEntities;
            when 54 => return new afrl.cmasi.FlightDirectorAction.FlightDirectorAction;
            when 55 => return new afrl.cmasi.WeatherReport.WeatherReport;
            when 56 => return new afrl.cmasi.FollowPathCommand.FollowPathCommand;
            when 57 => return new afrl.cmasi.PathWaypoint.PathWaypoint;
            when 58 => return new afrl.cmasi.StopMovementAction.StopMovementAction;
            when 59 => return new afrl.cmasi.WaypointTransfer.WaypointTransfer;
            when 60 => return new afrl.cmasi.PayloadStowAction.PayloadStowAction;
            when others => return null;
         end case;
      else
         return null;
      end if;
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
