package body afrl.cmasi.gimbalconfiguration is

   function getFullLmcpTypeName(this : GimbalConfiguration'Class) return String is ("afrl.cmasi.gimbalconfiguration.GimbalConfiguration");

   function getLmcpTypeName (this : GimbalConfiguration'Class) return String is ("GimbalConfiguration");

   function getLmcpType (this : GimbalConfiguration'Class) return UInt32_t is (CmasiEnum'Pos(GIMBALCONFIGURATION_ENUM));

   function getSupportedPointingModes(this : GimbalConfiguration'Class) return Vect_GimbalPointingModeEnum_Acc is (this.SupportedPointingModes);

   function getMinAzimuth (this : GimbalConfiguration'Class) return Float_t is (this.MinAzimuth);

   procedure setMinAzimuth(this : out GimbalConfiguration'Class; MinAzimuth : in Float_t) is
   begin
      this.MinAzimuth := MinAzimuth;
   end setMinAzimuth;

   function getMaxAzimuth (this : GimbalConfiguration'Class) return Float_t is (this.MaxAzimuth);

   procedure setMaxAzimuth(this : out GimbalConfiguration'Class; MaxAzimuth : in Float_t) is
   begin
      this.MaxAzimuth := MaxAzimuth;
   end setMaxAzimuth;

   function getIsAzimuthClamped(this : GimbalConfiguration'Class) return Boolean is (this.IsAzimuthClamped);

   procedure setIsAzimuthClamped(this : out GimbalConfiguration'Class; IsAzimuthClamped : in Boolean) is
   begin
      this.IsAzimuthClamped := IsAzimuthClamped;
   end setIsAzimuthClamped;

   function getMinElevation(this : GimbalConfiguration'Class) return Float_t is (this.MinElevation);

   procedure setMinElevation(this : out GimbalConfiguration'Class; MinElevation : in Float_t) is
   begin
      this.MinElevation := MinElevation;
   end setMinElevation;

   function getMaxElevation(this : GimbalConfiguration'Class) return Float_t is (this.MaxElevation);

   procedure setMaxElevation(this : out GimbalConfiguration'Class; MaxElevation : in Float_t) is
   begin
      this.MaxElevation := MaxElevation;
   end setMaxElevation;

   function getIsElevationClamped(this : GimbalConfiguration'Class) return Boolean is (this.IsElevationClamped);

   procedure setIsElevationClamped(this : out GimbalConfiguration'Class; IsElevationClamped : in Boolean) is
   begin
      this.IsElevationClamped := IsElevationClamped;
   end setIsElevationClamped;

   function getMinRotation (this : GimbalConfiguration'Class) return Float_t is (this.MinRotation);

   procedure getMinRotation(this : out GimbalConfiguration'Class; MinRotation : in Float_t) is
   begin
      this.MinRotation := MinRotation;
   end getMinRotation;

   function getMaxRotation(this : GimbalConfiguration'Class) return Float_t is (this.MaxRotation);

   procedure setMaxRotation(this : out GimbalConfiguration'Class; MaxRotation : in Float_t) is
   begin
      this.MaxRotation := MaxRotation;
   end setMaxRotation;

   function getIsRotationClamped(this : GimbalConfiguration'Class) return Boolean is (this.IsRotationClamped);

   procedure setIsRotationClamped(this : out GimbalConfiguration'Class; IsRotationClamped : in Boolean) is
   begin
      this.IsRotationClamped := IsRotationClamped;
   end setIsRotationClamped;

   function getMaxAzimuthSlewRate(this : GimbalConfiguration'Class) return Float_t is (this.MaxAzimuthSlewRate);

   procedure setMaxAzimuthSlewRate(this : out GimbalConfiguration'Class; MaxAzimuthSlewRate : in Float_t) is
   begin
      this.MaxAzimuthSlewRate := MaxAzimuthSlewRate;
   end setMaxAzimuthSlewRate;

   function getMaxElevationSlewRate(this : GimbalConfiguration'Class) return Float_t is (this.MaxElevationSlewRate);

   procedure setMaxElevationSlewRate(this : out GimbalConfiguration'Class; MaxElevationSlewRate : in Float_t) is
   begin
      this.MaxElevationSlewRate := MaxElevationSlewRate;
   end setMaxElevationSlewRate;

   function getMaxRotationRate(this : GimbalConfiguration'Class) return Float_t is (this.MaxRotationRate);

   procedure setMaxRotationRate(this : out GimbalConfiguration'Class; MaxRotationRate : in Float_t) is
   begin
      this.MaxRotationRate := MaxRotationRate;
   end setMaxRotationRate;

   function getContainedPayloadList(this : GimbalConfiguration'Class) return Vect_Int64_t_Acc is (this.ContainedPayloadList);

end afrl.cmasi.gimbalconfiguration;
