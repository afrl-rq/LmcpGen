with avtas.lmcp.types; use avtas.lmcp.types;
with afrl.cmasi.object; use afrl.cmasi.object;
with afrl.cmasi.enumerations; use afrl.cmasi.enumerations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package afrl.cmasi.keyValuePair is
   
   type KeyValuePair is new afrl.cmasi.object.Object with private;
   type KeyValuePair_Acc is access all KeyValuePair;
   -- Technically, nothing inherits from this, so we don't need a class access type
   type KeyValuePair_Class_Acc is access all KeyValuePair'Class;
   
   function getFullLmcpTypeName(this : KeyValuePair) return String;
   function getLmcpTypeName(this : KeyValuePair) return String;
   function getLmcpType(this : KeyValuePair) return UInt32_t;
   function getKey(this : KeyValuePair'Class) return Unbounded_String;
   procedure setKey(this : out KeyValuePair'Class; Key : in Unbounded_String);
   function getValue(this : KeyValuePair'Class) return Unbounded_String;
   procedure setValue(this : out KeyValuePair'Class; Value : in Unbounded_String);

private
   
   type KeyValuePair is new afrl.cmasi.object.Object with record
      Key : Unbounded_String;
      Value : Unbounded_String;
   end record;

end afrl.cmasi.keyValuePair;
