with avtas.lmcp.types; use avtas.lmcp.types;

package avtas.lmcp.object is
   
   type Object is tagged null record;
   type Object_Acc is access Object;
   
   function clone(this, that: Object_Acc) return Object_Acc is abstract;
   
   function "="(this, that: Object) return Boolean is (True);
     
   function getLmcpTypeName(this : Object) return String is ("Object");
   
   function getFullLmcpTypeName(this : Object) return String is ("avtas.lmcp.object.Object");
   
   function getLmcpType(this : Object'Class) return Int32_t is abstract;
   
   function getSeriesName(this : Object'Class) return String is abstract;
   
   function getSeriesNameAsLong(this : Object'Class) return Int64_t is abstract;
   
   function getSeriesVersion(this : Object'Class) return UInt16_t is abstract;
   
end avtas.lmcp.object;
