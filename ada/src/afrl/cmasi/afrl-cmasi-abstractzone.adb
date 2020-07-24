package body afrl.cmasi.abstractZone is

   function getFullLmcpTypeName(this : AbstractZone'Class) return String is ("afrl.cmasi.abstractZone.AbstractZone");

   function getLmcpTypeName(this : AbstractZone'Class) return String is ("AbstractZone");

   function getLmcpType(this : AbstractZone'Class) return UInt32_t is (CMASIEnum'Pos(ABSTRACTZONE_ENUM));

   function getZoneId(this : AbstractZone'Class) return Int64_t is (this.ZoneID);

   procedure setZoneId(this : out AbstractZone'Class; ZoneId : in Int64_t) is
   begin
      this.ZoneID := ZoneId;
   end setZoneId;

   function getMinAltitude (this : AbstractZone'Class) return Float_t is (this.MinAltitude);

   procedure setMinAltitude(this : out AbstractZone'Class; MinAltitude : in Float_t) is
   begin
      this.MinAltitude := MinAltitude;
   end setMinAltitude;

   function getMinAltitudeType(this : AbstractZone'Class) return AltitudeTypeEnum is (this.MinAltitudeType);

   procedure setMinAltitudeType(this : out AbstractZone'Class; MinAltitudeType : in AltitudeTypeEnum) is
   begin
      this.MinAltitudeType := MinAltitudeType;
   end setMinAltitudeType;

   function getMaxAltitude (this : AbstractZone'Class) return Float_t is (this.MaxAltitude);

   procedure setMaxAltitude(this : out AbstractZone'Class; MaxAltitude : in Float_t) is
   begin
      this.MaxAltitude := MaxAltitude;
   end setMaxAltitude;

   function getMaxAltitudeType(this : AbstractZone'Class) return AltitudeTypeEnum is (this.MaxAltitudeType);

   procedure setMaxAltitudeType(this : out AbstractZone'Class; MaxAltitudeType : in AltitudeTypeEnum) is
   begin
      this.MaxAltitudeType := MaxAltitudeType;
   end setMaxAltitudeType;

   function getAffectedAircraft(this : AbstractZone'Class) return Vect_Int64_t_Acc is (this.AffectedAircraft);

   function getStartTime (this : AbstractZone'Class) return Int64_t is (this.StartTime);

   procedure setStartTime(this : out AbstractZone'Class; StartTime : in Int64_t) is
   begin
      this.StartTime := StartTime;
   end setStartTime;

   function getEndTime (this : AbstractZone'Class) return Int64_t is (this.EndTime);

   procedure setEndTime(this : out AbstractZone'Class; EndTime : in Int64_t) is
   begin
      this.EndTime := EndTime;
   end setEndTime;

   function getPadding (this : AbstractZone'Class) return Float_t is (this.Padding);

   procedure setPadding(this : out AbstractZone'Class; Padding : in Float_t) is
   begin
      this.Padding := Padding;
   end setPadding;

   function getLabel (this : AbstractZone'Class) return Unbounded_String is (this.Label);

   procedure setLabel(this : out AbstractZone'Class; Label : in Unbounded_String) is
   begin
      this.Label := Label;
   end setLabel;

   function getBoundary(this : AbstractZone'Class) return AbstractGeometry_Acc is (this.Boundary);

   procedure setBoundary(this : out AbstractZone'Class; Boundary : in AbstractGeometry_Acc) is
   begin
      this.Boundary := Boundary;
   end setBoundary;

end afrl.cmasi.abstractZone;
