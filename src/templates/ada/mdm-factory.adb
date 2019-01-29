-<include_all_series_headers>-
with Ada.Unchecked_Conversion;
with GNAT.Byte_Swapping;

package body -<full_series_name_dots>-.factory is

   function packMessage(rootObject : in avtas.lmcp.object.Object_Any; enableChecksum : in Boolean) return ByteBuffer is
      msgSize : UInt32_t;
   begin
      -- Allocate space for message, with 15 extra bytes for
      --  Existence (1 byte), series name (8 bytes), type (4 bytes), version number (2 bytes)
      msgSize := rootObject.calculatePackedSize + 15;
      declare
         buffer : ByteBuffer(HEADER_SIZE + msgSize + CHECKSUM_SIZE);
      begin
         -- add header values
         Put_Int32_t(LMCP_CONTROL_STR, buffer);
         Put_UInt32_t(msgSize, buffer);

         -- If root object is null, pack a 0; otherwise, add root object
         if(rootObject = null) then
            Put_Boolean(False, buffer);
         else
            Put_Boolean(True, buffer);
            Put_Int64_t(rootObject.getSeriesNameAsLong, buffer);
            Put_UInt32_t(rootObject.getLmcpType, buffer);
            Put_UInt16_t(rootObject.getSeriesVersion, buffer);
            pack(rootObject, buffer);
         end if;

         -- add checksum if enabled
         Put_UInt32_t((if enableChecksum then calculateChecksum(buffer) else 0), buffer);
         return buffer;
      end;
   end packMessage;

   procedure getObject(buffer : in out ByteBuffer; output : out avtas.lmcp.object.Object_Any) is
      ctrlStr : Int32_t;
      msgSize : UInt32_t;
      msgExists : Boolean;
      seriesId : Int64_t;
      msgType : Uint32_t;
      version : Uint16_t;
   begin
      -- TODO: add some kind of warning/error messages for each null case
      if buffer.Capacity < HEADER_SIZE + CHECKSUM_SIZE then
         output := null;
      else
         Get_Int32_t(buffer, ctrlStr);
         if ctrlStr /= LMCP_CONTROL_STR then
            output := null;
         else
            Get_UInt32_t(buffer, msgSize);
            if buffer.Capacity < msgSize then
               output := null;
            elsif(validate(buffer) = False) then
               output := null;
            else
               Get_Boolean(buffer, msgExists);
               if(msgExists = False) then
                  output := null;
               else
                  Get_Int64_t(buffer, seriesId);
                  Get_UInt32_t(buffer, msgType);
                  Get_UInt16_t(buffer, version);
                  output := createObject(seriesId, msgType, version);
                  if (output /= null) then
                     unpack(buffer, output);
                  end if;
               end if;
            end if;
         end if;
      end if;
   end getObject;

   function createObject(seriesId : in Int64_t; msgType : in UInt32_t; version: in UInt16_t) return avtas.lmcp.object.Object_Any is
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

   function calculateChecksum (buffer : in ByteBuffer) return UInt32_t is
      sum : UInt32_t := 0;
      function IntToByteArray is new Ada.Unchecked_Conversion(Source => UInt32_t, Target => ByteArray4);
      function ByteArrayToInt is new Ada.Unchecked_Conversion(Source => ByteArray4, Target => UInt32_t);
   begin
      for i in 1 .. buffer.Capacity - CHECKSUM_SIZE loop
         sum := sum + UInt32_t(buffer.Buf(i));
      end loop;
      -- The C++ code does the following, but why? It seems like a no-op to me
      -- Can't we just return sum?
      -- return (ByteArrayToInt(IntToByteArray(sum) & IntToByteArray(UInt32_t(16#FFFFFFFF#))));
      return sum;
   end calculateChecksum;

   function getObjectSize (buffer : in ByteBuffer) return UInt32_t is
      function ByteArrayToInt is new Ada.Unchecked_Conversion(Source => ByteArray4, Target => UInt32_t);
   begin
      return ByteArrayToInt(buffer.Buf(5 .. 8));
   end getObjectSize;

   function validate(buffer : in ByteBuffer) return Boolean is
      function ByteArrayToInt is new Ada.Unchecked_Conversion(Source => ByteArray4, Target => UInt32_t);
      subtype SwapType is ByteArray4;
      function SwapBytes is new GNAT.Byte_Swapping.Swapped4 (swapType);
      sum : UInt32_t;
   begin
      sum := calculateChecksum(buffer);
      return sum = 0 or else sum = ByteArrayToInt(SwapBytes(buffer.Buf(buffer.Buf'Last - 3 .. buffer.Buf'Last)));
   end validate;

end -<full_series_name_dots>-.factory;
