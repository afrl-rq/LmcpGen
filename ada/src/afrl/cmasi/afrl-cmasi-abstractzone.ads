with avtas.lmcp.types; use avtas.lmcp.types;
with afrl.cmasi.object; use afrl.cmasi.object;
with afrl.cmasi.enumerations; use afrl.cmasi.enumerations;
with afrl.cmasi.AbstractGeometry; use afrl.cmasi.AbstractGeometry;
with ada.Containers.Vectors;
with ada.Strings.Unbounded; use ada.Strings.Unbounded;

package afrl.cmasi.abstractZone is
   
   type AbstractZone is new afrl.cmasi.object.Object with private;
   type AbstractZone_Acc is access all AbstractZone;
   
   package Vect_Int64_t is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Int64_t);
   type Vect_Int64_t_Acc is access all Vect_Int64_t.Vector;

   function getFullLmcpTypeName(this : AbstractZone'Class) return String;
   function getLmcpTypeName(this : AbstractZone'Class) return String;
   function getLmcpType(this : AbstractZone'Class) return UInt32_t;
   function getZoneId(this : AbstractZone'Class) return Int64_t;
   procedure setZoneId(this : out AbstractZone'Class; ZoneId : in Int64_t);
   function getMinAltitude(this : AbstractZone'Class) return Float_t;
   procedure setMinAltitude(this : out AbstractZone'Class; MinAltitude : in Float_t);
   function getMinAltitudeType(this : AbstractZone'Class) return AltitudeTypeEnum;
   procedure setMinAltitudeType(this : out AbstractZone'Class; MinAltitudeType : in AltitudeTypeEnum);
   function getMaxAltitude(this : AbstractZone'Class) return Float_t;
   procedure setMaxAltitude(this : out AbstractZone'Class; MaxAltitude : in Float_t);
   function getMaxAltitudeType(this : AbstractZone'Class) return AltitudeTypeEnum;
   procedure setMaxAltitudeType(this : out AbstractZone'Class; MaxAltitudeType : in AltitudeTypeEnum);
   function getAffectedAircraft(this : AbstractZone'Class) return Vect_Int64_t_Acc;
   function getStartTime(this : AbstractZone'Class) return Int64_t;
   procedure setStartTime(this : out AbstractZone'Class; StartTime : in Int64_t);
   function getEndTime(this : AbstractZone'Class) return Int64_t;
   procedure setEndTime(this : out AbstractZone'Class; EndTime : in Int64_t);
   function getPadding(this : AbstractZone'Class) return Float_t;
   procedure setPadding(this : out AbstractZone'Class; Padding : in Float_t);
   function getLabel(this : AbstractZone'Class) return Unbounded_String;
   procedure setLabel(this : out AbstractZone'Class; Label : in Unbounded_String);
   function getBoundary(this : AbstractZone'Class) return AbstractGeometry_Acc;
   procedure setBoundary(this : out AbstractZone'Class; Boundary : in AbstractGeometry_Acc);
   
   
private
   
   type AbstractZone is new afrl.cmasi.object.Object with record 
      ZoneID : Int64_t := 0;
      MinAltitude : Float_t := 0.0;
      MinAltitudeType : AltitudeTypeEnum := AGL;
      MaxAltitude : Float_t := 0.0;
      MaxAltitudeType : AltitudeTypeEnum := MSL;
      AffectedAircraft : Vect_Int64_t_Acc := new Vect_Int64_t.Vector;
      StartTime : Int64_t := 0;
      EndTime : Int64_t := 0;
      Padding : Float_t := 0.0;
      Label : Unbounded_String;
      Boundary : AbstractGeometry_Acc; 
   end record;

end afrl.cmasi.abstractZone;
