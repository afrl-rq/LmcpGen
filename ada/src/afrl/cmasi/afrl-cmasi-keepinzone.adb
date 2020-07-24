package body afrl.cmasi.keepInZone is

   function getFullLmcpTypeName(this : KeepInZone) return String is ("afrl.cmasi.keepInZone.KeepInZone");

   function getLmcpTypeName(this : KeepInZone) return String is ("KeepInZone");

   function getLmcpType (this : KeepInZone) return UInt32_t is (CmasiEnum'Pos(KEEPINZONE_ENUM));

end afrl.cmasi.keepInZone;
