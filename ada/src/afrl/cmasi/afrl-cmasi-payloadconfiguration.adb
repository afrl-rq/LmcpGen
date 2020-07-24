package body afrl.cmasi.payloadConfiguration is

   function getFullLmcpTypeName (this : PayloadConfiguration) return String is ("afrl.cmasi.payloadConfiguration.PayloadConfiguration");

   function getLmcpTypeName (this : PayloadConfiguration) return String is ("PayloadConfigurations");

   function getLmcpType (this : PayloadConfiguration) return UInt32_t is (CMASIEnum'Pos(PAYLOADCONFIGURATION_ENUM));

   function getPayloadID (this : PayloadConfiguration'Class) return Int64_t is (this.PayloadID);

   procedure setPayloadID(this : out PayloadConfiguration'Class; PayloadID : in Int64_t) is
   begin
      this.PayloadID := PayloadID;
   end setPayloadID;

   function getPayloadKind(this : PayloadConfiguration'Class) return Unbounded_String is (this.PayloadKind);

   procedure setPayloadKind(this : out PayloadConfiguration'Class; PayloadKind : in Unbounded_String) is
   begin
      this.PayloadKind := PayloadKind;
   end setPayloadKind;

   function getParameters(this : PayloadConfiguration'Class) return Vect_Unbounded_String_Acc is (this.Parameters);

end afrl.cmasi.payloadConfiguration;
