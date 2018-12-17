with avtas.lmcp.object; use avtas.lmcp.object;
with avtas.lmcp.types; use avtas.lmcp.types;
with afrl.cmasi.enumerations; use afrl.cmasi.enumerations;

package afrl.cmasi.object is
   
   type Object is abstract new avtas.lmcp.object.Object with private;
   
   function getSeriesVersion(this : Object) return UInt16_t is (3);
   function getSeriesName(this : Object) return String is ("CMASI");
   function getSeriesNameAsLong(this : Object) return Int64_t is (4849604199710720000);
   
private
   
   type Object is new avtas.lmcp.object.Object with null record;

end afrl.cmasi.object;
