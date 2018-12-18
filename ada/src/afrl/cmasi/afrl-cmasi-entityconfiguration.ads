with afrl.cmasi.object; use afrl.cmasi.object;
with afrl.cmasi.enumerations; use afrl.cmasi.enumerations;
with avtas.lmcp.types; use avtas.lmcp.types;
with afrl.cmasi.payloadConfiguration; use afrl.cmasi.payloadConfiguration;
with afrl.cmasi.keyValuePair; use afrl.cmasi.keyValuePair;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package afrl.cmasi.entityConfiguration is

   type EntityConfiguration is new afrl.cmasi.object.Object with private;
   type EntityConfiguration_Acc is access all EntityConfiguration;
   type EntityConfiguration_Class_Acc is access all EntityConfiguration'Class;
   
   package Vect_PayloadConfiguration_Class_Acc is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => PayloadConfiguration_Class_Acc);
   type Vect_PayloadConfiguration_Class_Acc_Acc is access all Vect_PayloadConfiguration_Class_Acc.Vector;
   
   package Vect_KeyValuePair_Acc is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => KeyValuePair_Acc);
   type Vect_KeyValuePair_Acc_Acc is access all Vect_KeyValuePair_Acc.Vector;
   
   function getFullLmcpTypeName(this : EntityConfiguration) return String;
   function getLmcpTypeName(this : EntityConfiguration) return String;
   function getLmcpType(this : EntityConfiguration) return UInt32_t;
   function getID(this : EntityConfiguration'Class) return Int64_t;
   procedure setID(this : out EntityConfiguration'Class; ID : in Int64_t);
   function getAffiliation(this : EntityConfiguration'Class) return Unbounded_String;
   procedure setAffiliation(this : out EntityConfiguration'Class; Affiliation : in Unbounded_String);
   function getEntityType(this : EntityConfiguration'Class) return Unbounded_String;
   procedure setEntityType(this : out EntityConfiguration'Class; EntityType : in Unbounded_String);
   function getLabel(this : EntityConfiguration'Class) return Unbounded_String;
   procedure setLabel(this : out EntityConfiguration'Class; Label : in Unbounded_String);
   function getNominalSpeed(this : EntityConfiguration'Class) return Float_t;
   procedure setNominalSpeed(this : out EntityConfiguration'Class; NominalSpeed : in Float_t);
   function getNominalAltitude(this : EntityConfiguration'Class) return Float_t;
   procedure setNominalAltitude(this : out EntityConfiguration'Class; NominalAltitude : in Float_t);
   function getNominalAltitudeType(this : EntityConfiguration'Class) return AltitudeTypeEnum;
   procedure setNominalAltitudeType(this : out EntityConfiguration'Class; NominalAltitudeType : in AltitudeTypeEnum);
   function getPayloadConfigurationList(this : EntityConfiguration'Class) return Vect_PayloadConfiguration_Class_Acc_Acc;
   function getInfo(this : EntityConfiguration'Class) return Vect_KeyValuePair_Acc_Acc;
   
private
   
   type EntityConfiguration is new afrl.cmasi.object.Object with record
      ID : Int64_t := 0;
      Affiliation : Unbounded_String := To_Unbounded_String("Unknown");
      EntityType : Unbounded_String;
      Label : Unbounded_String;
      NominalSpeed : Float_t := 0.0;
      NominalAltitude : Float_t := 0.0;
      NominalAltitudeType : AltitudeTypeEnum := AGL;
      PayloadConfigurationList : Vect_PayloadConfiguration_Class_Acc_Acc := new Vect_PayloadConfiguration_Class_Acc.Vector;
      Info : Vect_KeyValuePair_Acc_Acc := new Vect_KeyValuePair_Acc.Vector;
   end record;

end afrl.cmasi.entityConfiguration;
