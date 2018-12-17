with avtas.lmcp.types; use avtas.lmcp.types;
with afrl.cmasi.object; use afrl.cmasi.object;
with afrl.cmasi.enumerations; use afrl.cmasi.enumerations;

package afrl.cmasi.location3D is

   type Location3D is new afrl.cmasi.object.Object with private;
   type Location3D_Acc is access all Location3D;
   
   function getFullLmcpTypeName(this : Location3D) return String;
   function getLmcpTypeName(this : Location3D) return String;
   function getLmcpType(this : Location3D) return UInt32_t;
   function getLatitude(this : Location3D'Class) return Double_t;
   procedure setLatitude(this : out Location3D'Class; Latitude : in Double_t);
   function getLongitude(this : Location3D'Class) return Double_t;
   procedure setLongitude(this : out Location3D'Class; Longitude : in Double_t);
   function getAltitude(this : Location3D'Class) return Float_t;
   procedure setAltitude(this : out Location3D'Class; Altitude : in Float_t);
   function getAltitudeType(this : Location3D'Class) return AltitudeTypeEnum;
   procedure setAltitudeType(this : out Location3D'Class; AltitudeType : in AltitudeTypeEnum);
   
private
   
   type Location3D is new afrl.cmasi.object.Object with record
      Latitude : Double_t := 0.0;
      Longitude : Double_t := 0.0;
      Altitude : Float_t := 0.0;
      AltitudeType : AltitudeTypeEnum := MSL;
   end record;
   
end afrl.cmasi.location3D;
