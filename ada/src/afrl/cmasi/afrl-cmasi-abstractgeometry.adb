package body afrl.cmasi.abstractGeometry is

   function getFullLmcpTypeName(this : AbstractGeometry) return String is ("afrl.cmasi.abstractGeometry.AbstractGeometry");
   function getLmcpTypeName(this : AbstractGeometry) return String is ("AbstractGeometry");
   function getLmcpType(this : AbstractGeometry) return UInt32_t is (CMASIEnum'Pos(ABSTRACTGEOMETRY_ENUM));

end afrl.cmasi.abstractGeometry;
