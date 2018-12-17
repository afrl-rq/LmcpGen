package body afrl.cmasi.keyValuePair is

   function getFullLmcpTypeName (this : KeyValuePair) return String is ("afrl.cmasi.keyValuePair.KeyValuePair");

   function getLmcpTypeName (this : KeyValuePair) return String is ("KeyValuePair");

   function getLmcpType (this : KeyValuePair) return UInt32_t is (CMASIEnum'Pos(KEYVALUEPAIR_ENUM));

   function getKey (this : KeyValuePair'Class) return Unbounded_String is (this.Key);

   procedure setKey(this : out KeyValuePair'Class; Key : in Unbounded_String) is
   begin
      this.Key := Key;
   end setKey;

   function getValue (this : KeyValuePair'Class) return Unbounded_String is (this.Value);

   procedure setValue (this : out KeyValuePair'Class; Value : in Unbounded_String) is
   begin
      this.Value := Value;
   end setValue;

end afrl.cmasi.keyValuePair;
