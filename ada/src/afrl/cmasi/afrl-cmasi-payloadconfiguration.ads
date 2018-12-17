with avtas.lmcp.types; use avtas.lmcp.types;
with afrl.cmasi.object; use afrl.cmasi.object;
with afrl.cmasi.enumerations; use afrl.cmasi.enumerations;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package afrl.cmasi.payloadConfiguration is

   type PayloadConfiguration is new afrl.cmasi.object.Object with private;
   type PayloadConfiguration_Acc is access all PayloadConfiguration;
   type PayloadConfiguration_Class_Acc is access all PayloadConfiguration'Class;
   
   package Vect_Unbounded_String is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Unbounded_String);
   type Vect_Unbounded_String_Acc is access all Vect_Unbounded_String.Vector;
   
   function getFullLmcpTypeName(this : PayloadConfiguration) return String;
   function getLmcpTypeName(this : PayloadConfiguration) return String;
   function getLmcpType(this : PayloadConfiguration) return UInt32_t;
   function getPayloadID(this : PayloadConfiguration'Class) return Int64_t;
   procedure setPayloadID(this : out PayloadConfiguration'Class; PayloadID : in Int64_t);
   function getPayloadKind(this : PayloadConfiguration'Class) return Unbounded_String;
   procedure setPayloadKind(this : out PayloadConfiguration'Class; PayloadKind : in Unbounded_String);
   function getParameters(this : PayloadConfiguration'Class) return Vect_Unbounded_String_Acc;
   
private
   
   type PayloadConfiguration is new afrl.cmasi.object.Object with record
      PayloadID : Int64_t := 0;
      PayloadKind : Unbounded_String;
      Parameters : Vect_Unbounded_String_Acc := new Vect_Unbounded_String.Vector;
   end record;

end afrl.cmasi.payloadConfiguration;
