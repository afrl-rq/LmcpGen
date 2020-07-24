package body afrl.cmasi.location3D is

   function getFullLmcpTypeName(this : Location3D) return String is ("afrl.cmasi.location3D.Location3D");

   function getLmcpTypeName(this : Location3D) return String is ("Location3D");

   function getLmcpType(this : Location3D) return UInt32_t is (CMASIEnum'Pos(LOCATION3D_ENUM));

   function getLatitude (this : Location3D'Class) return Double_t is (this.Latitude);

   procedure setLatitude(this : out Location3D'Class; Latitude : in Double_t) is
   begin
      this.Latitude := Latitude;
   end setLatitude;

   function getLongitude(this : Location3D'Class) return Double_t is (this.Longitude);

   procedure setLongitude(this : out Location3D'Class; Longitude : in double_t) is
   begin
      this.Longitude := Longitude;
   end setLongitude;

   function getAltitude(this : Location3D'Class) return Float_t is (this.Altitude);

   procedure setAltitude(this : out Location3D'Class; Altitude : in Float_t) is
   begin
      this.Altitude := Altitude;
   end setAltitude;

   function getAltitudeType(this : Location3D'Class) return AltitudeTypeEnum is (this.AltitudeType);

   procedure setAltitudeType(this : out Location3D'Class; AltitudeType : in AltitudeTypeEnum) is
   begin
      this.AltitudeType := AltitudeType;
   end setAltitudeType;

end afrl.cmasi.location3D;
