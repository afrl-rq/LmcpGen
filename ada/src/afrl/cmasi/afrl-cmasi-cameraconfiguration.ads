with avtas.lmcp.types; use avtas.lmcp.types;
with afrl.cmasi.object; use afrl.cmasi.object;
with afrl.cmasi.enumerations; use afrl.cmasi.enumerations;
with afrl.cmasi.payloadConfiguration; use afrl.cmasi.payloadConfiguration;
with Ada.Containers.Vectors;

package afrl.cmasi.cameraConfiguration is

   type CameraConfiguration is new afrl.cmasi.payloadConfiguration.PayloadConfiguration with private;
   type CameraConfiguration_Acc is access all CameraConfiguration;
   
   package Vect_Float_t is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Float_t);
   type Vect_Float_t_Acc is access all Vect_Float_t.Vector;
   
   function getFullLmcpTypeName(this : CameraConfiguration'Class) return String;
   function getLmcpTypeName(this : CameraConfiguration'Class) return String;
   function getLmcpType(this : CameraConfiguration'Class) return UInt32_t;
   function getSupportedWavelengthBand(this : CameraConfiguration'Class) return WavelengthBandEnum;
   procedure setSupportedWavelengthBand(this : out CameraConfiguration'Class; SupportedWavelengthBand : in WavelengthBandEnum);
   function getFieldOfViewMode(this : CameraConfiguration'Class) return FOVOperationModeEnum;
   procedure setFieldOfViewMode(this : out CameraConfiguration'Class; FieldOfViewMode : in FOVOperationModeEnum);
   function getMinHorizontalFieldOfView(this : CameraConfiguration'Class) return Float_t;
   procedure setMinHorizontalFieldOfView(this : out CameraConfiguration'Class; MinHorizontalFieldOfView : in Float_t);
   function getMaxHorizontalFieldOfView(this : CameraConfiguration'Class) return Float_t;
   procedure setMaxHorizontalFieldOfView(this : out CameraConfiguration'Class; MaxHorizontalFieldOfView : in Float_t);
   function getDiscreteHorizontalFieldOfViewList(this : CameraConfiguration'Class) return Vect_Float_t_Acc;
   function getVideoStreamHorizontalResolution(this : CameraConfiguration'Class) return UInt32_t;
   procedure setVideoStreamHorizontalResolution(this : out CameraConfiguration'Class; VideoStreamHorizontalResolution : in UInt32_t);
   function getVideoStreamVerticalResolution(this : CameraConfiguration'Class) return UInt32_t;
   procedure setVideoStreamVerticalResolution(this : out CameraConfiguration'Class; VideoStreamVerticalResolution : in UInt32_t);

private
   
   type CameraConfiguration is new afrl.cmasi.payloadConfiguration.PayloadConfiguration with record
      SupportedWavelengthBand : WavelengthBandEnum := EO;
      FieldOfViewMode : FOVOperationModeEnum := Continuous;
      MinHorizontalFieldOfView : Float_t := 0.0;
      MaxHorizontalFieldOfView : Float_t := 0.0;
      DiscreteHorizontalFieldOfViewList : Vect_Float_t_Acc;
      VideoStreamHorizontalResolution : UInt32_t := 0;
      VideoStreamVerticalResolution : UInt32_t := 0;
   end record; 
   
end afrl.cmasi.cameraConfiguration;
