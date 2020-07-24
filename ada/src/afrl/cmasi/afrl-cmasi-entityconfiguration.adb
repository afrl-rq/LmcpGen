package body afrl.cmasi.entityConfiguration is

   function getFullLmcpTypeName (this : EntityConfiguration) return String is ("afrl.cmasi.entityConfiguration.EntityConfiguration");

   function getLmcpTypeName (this : EntityConfiguration) return String is ("EntityConfiguration");

   function getLmcpType (this : EntityConfiguration) return UInt32_t is (CmasiEnum'Pos(ENTITYCONFIGURATION_ENUM));

   function getID (this : EntityConfiguration'Class) return Int64_t is (this.ID);

   procedure setID (this : out EntityConfiguration'Class; ID : in Int64_t) is
   begin
      this.ID := ID;
   end setID;

   function getAffiliation(this : EntityConfiguration'Class) return Unbounded_String is (this.Affiliation);

   procedure setAffiliation(this : out EntityConfiguration'Class; Affiliation : in Unbounded_String) is
   begin
      this.Affiliation := Affiliation;
   end setAffiliation;

   function getEntityType(this : EntityConfiguration'Class) return Unbounded_String is (this.EntityType);

   procedure setEntityType(this : out EntityConfiguration'Class; EntityType : in Unbounded_String) is
   begin
      this.EntityType := EntityType;
   end setEntityType;

   function getLabel(this : EntityConfiguration'Class) return Unbounded_String is (this.Label);

   procedure setLabel(this : out EntityConfiguration'Class; Label : in Unbounded_String) is
   begin
      this.Label := Label;
   end setLabel;

   function getNominalSpeed(this : EntityConfiguration'Class) return Float_t is (this.NominalSpeed);

   procedure setNominalSpeed(this : out EntityConfiguration'Class; NominalSpeed : in Float_t) is
   begin
      this.NominalSpeed := NominalSpeed;
   end setNominalSpeed;

   function getNominalAltitude(this : EntityConfiguration'Class) return Float_t is (this.NominalAltitude);

   procedure setNominalAltitude(this : out EntityConfiguration'Class; NominalAltitude : in Float_t) is
   begin
      this.NominalAltitude := NominalAltitude;
   end setNominalAltitude;

   function getNominalAltitudeType(this : EntityConfiguration'Class) return AltitudeTypeEnum is (this.NominalAltitudeType);

   procedure setNominalAltitudeType(this : out EntityConfiguration'Class; NominalAltitudeType : in AltitudeTypeEnum) is
   begin
      this.NominalAltitudeType := NominalAltitudeType;
   end setNominalAltitudeType;

   function getPayloadConfigurationList(this : EntityConfiguration'Class) return Vect_PayloadConfiguration_Class_Acc_Acc is (this.PayloadConfigurationList);

   function getInfo(this : EntityConfiguration'Class) return Vect_KeyValuePair_Acc_Acc is (this.Info);

end afrl.cmasi.entityConfiguration;
