with avtas.lmcp.types; use avtas.lmcp.types;
with afrl.cmasi.object; use afrl.cmasi.object;
with afrl.cmasi.enumerations; use afrl.cmasi.enumerations;
with afrl.cmasi.payloadConfiguration; use afrl.cmasi.payloadConfiguration;
with Ada.Containers.Vectors;

package afrl.cmasi.gimbalconfiguration is

   type GimbalConfiguration is new afrl.cmasi.payloadConfiguration.PayloadConfiguration with private;
   type GimbalConfiguration_Acc is access all GimbalConfiguration;
   
   package Vect_GimbalPointingModeEnum is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => GimbalPointingModeEnum);
   type Vect_GimbalPointingModeEnum_Acc is access all Vect_GimbalPointingModeEnum.Vector;
   
   package Vect_Int64_t is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Int64_t);
   type Vect_Int64_t_Acc is access all Vect_Int64_t.Vector;
   
   function getFullLmcpTypeName(this : GimbalConfiguration'Class) return String;
   function getLmcpTypeName(this : GimbalConfiguration'Class) return String;
   function getLmcpType(this : GimbalConfiguration'Class) return UInt32_t;
   
         SupportedPointingModes : Vect_GimbalPointingModeEnum_Acc;
      MinAzimuth : Float_t := -180.0;
      MaxAzimuth : Float_t := 180.0;
      IsAzimuthClamped : Boolean := False;
      MinElevation: Float_t := -180.0;
      MaxElevation : Float_t := 180.0;
      IsElevationClamped : Boolean := False;
      MinRotation: Float_t := 0.0;
      MaxRotation : Float_t := 0.0;
      IsRotationClamped : Boolean := True;
      MaxAzimuthSlewRate : Float_t := 0.0;
      MaxElevationSlewRate : Float_t := 0.0;
      MaxRotationRate : Float_t := 0.0;
      ContainedPayloadList : Vect_Int64_t_Acc;
   
   function getSupportedPointingModes(this : GimbalConfiguration'Class) return Vect_GimbalPointingModeEnum_Acc;
   function getMinAzimuth(this : GimbalConfiguration'Class) return Float_t;
   procedure setMinAzimuth(this : out GimbalConfiguration'Class; MinAzimuth : in Float_t);
   function getMaxAzimuth(this : GimbalConfiguration'Class) return Float_t;
   procedure setMaxAzimuth(this : out GimbalConfiguration'Class; MaxAzimuth : in Float_t);
   function getIsAzimuthClamped(this : GimbalConfiguration'Class) return Boolean;
   procedure setIsAzimuthClamped(this : out GimbalConfiguration'Class; IsAzimuthClamped : in Boolean);
   function getMinElevation(this : GimbalConfiguration'Class) return Float_t;
   procedure setMinElevation(this : out GimbalConfiguration'Class; MinElevation : in Float_t);
   function getMaxElevation(this : GimbalConfiguration'Class) return Float_t;
   procedure setMaxElevation(this : out GimbalConfiguration'Class; MaxElevation : in Float_t);
   function getIsElevationClamped(this : GimbalConfiguration'Class) return Boolean;
   procedure setIsElevationClamped(this : out GimbalConfiguration'Class; IsElevationClamped : in Boolean);
   function getMinRotation(this : GimbalConfiguration'Class) return Float_t;
   procedure getMinRotation(this : out GimbalConfiguration'Class; MinRotation : in Float_t);
   function getMaxRotation(this : GimbalConfiguration'Class) return Float_t;
   procedure setMaxRotation(this : out GimbalConfiguration'Class; MaxRotation : in Float_t);
   function getIsRotationClamped(this : GimbalConfiguration'Class) return Boolean;
   procedure setIsRotationClamped(this : out GimbalConfiguration'Class; IsRotationClamped : in Boolean);
   function getMaxAzimuthSlewRate(this : GimbalConfiguration'Class) return Float_t;
   procedure setMaxAzimuthSlewRate(this : out GimbalConfiguration'Class; MaxAzimuthSlewRate : in Float_t);
   function getMaxElevationSlewRate(this : GimbalConfiguration'Class) return Float_t;
   procedure setMaxElevationSlewRate(this : out GimbalConfiguration'Class; MaxElevationSlewRate : in Float_t);
   function getMaxRotationRate(this : GimbalConfiguration'Class) return Float_t;
   procedure setMaxRotationRate(this : out GimbalConfiguration'Class; MaxRotationRate : in Float_t);
   function getContainedPayloadList(this : GimbalConfiguration'Class) return Vect_Int64_t_Acc;
   
private
   
   type GimbalConfiguration is new afrl.cmasi.payloadConfiguration.PayloadConfiguration with record
      SupportedPointingModes : Vect_GimbalPointingModeEnum_Acc;
      MinAzimuth : Float_t := -180.0;
      MaxAzimuth : Float_t := 180.0;
      IsAzimuthClamped : Boolean := False;
      MinElevation: Float_t := -180.0;
      MaxElevation : Float_t := 180.0;
      IsElevationClamped : Boolean := False;
      MinRotation: Float_t := 0.0;
      MaxRotation : Float_t := 0.0;
      IsRotationClamped : Boolean := True;
      MaxAzimuthSlewRate : Float_t := 0.0;
      MaxElevationSlewRate : Float_t := 0.0;
      MaxRotationRate : Float_t := 0.0;
      ContainedPayloadList : Vect_Int64_t_Acc;
   end record; 
   
end afrl.cmasi.gimbalconfiguration;
