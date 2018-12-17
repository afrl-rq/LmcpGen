with afrl.cmasi.object; use afrl.cmasi.object;
with afrl.cmasi.enumerations; use afrl.cmasi.enumerations;
with avtas.lmcp.types; use avtas.lmcp.types;
  
package afrl.cmasi.abstractGeometry is

   type AbstractGeometry is new afrl.cmasi.object.Object with private;
   type AbstractGeometry_Acc is access all AbstractGeometry;
   
   function getFullLmcpTypeName(this : AbstractGeometry) return String;
   function getLmcpTypeName(this : AbstractGeometry) return String;
   function getLmcpType(this : AbstractGeometry) return UInt32_t;
   
private
   
   type AbstractGeometry is new afrl.cmasi.object.Object with null record;
   
end afrl.cmasi.abstractGeometry;
