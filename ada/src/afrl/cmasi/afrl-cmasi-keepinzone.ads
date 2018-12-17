with avtas.lmcp.types; use avtas.lmcp.types;
with afrl.cmasi.object; use afrl.cmasi.object;
with afrl.cmasi.enumerations; use afrl.cmasi.enumerations;
with afrl.cmasi.abstractZone; use afrl.cmasi.abstractZone;

package afrl.cmasi.keepInZone is

   type KeepInZone is new AbstractZone.AbstractZone with private;
   type KeepInZone_Acc is access all KeepInZone;
   
   function getFullLmcpTypeName(this : KeepInZone) return String;
   function getLmcpTypeName(this : KeepInZone) return String;
   function getLmcpType(this : KeepInZone) return UInt32_t;
   
private
   
   type KeepInZone is new AbstractZone.AbstractZone with null record;

end afrl.cmasi.keepInZone;
