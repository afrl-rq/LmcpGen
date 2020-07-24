package body afrl.cmasi.operatingRegion is

   function getFullLmcpTypeName(this : OperatingRegion) return String is ("afrl.cmasi.operatingRegion.OperatingRegion");

   function getLmcpTypeName(this : OperatingRegion) return String is ("OperatingRegion");

   function getLmcpType(this : OperatingRegion) return UInt32_t is (CmasiEnum'Pos(OPERATINGREGION_ENUM));

   function getID(this : OperatingRegion'Class) return Int64_t is (this.ID);

   procedure setID(this : out OperatingRegion'Class; ID : in Int64_t) is
   begin
      this.ID := ID;
   end setID;

   function getKeepInAreas(this : OperatingRegion'Class) return Vect_Int64_t_Acc is (this.KeepInAreas);

   function getKeepOutAreas(this : OperatingRegion'Class) return Vect_Int64_t_Acc is (this.KeepOutAreas);

end afrl.cmasi.operatingRegion;
