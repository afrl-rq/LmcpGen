with avtas.lmcp.types; use avtas.lmcp.types;

package avtas.lmcp.object is
   
   type Object is tagged null record;
   type Object_Acc is access Object;
   type Object_Any is access Object'Class;
   
   function clone(this, that: Object_Acc) return Object_Acc is abstract;
   
   function "="(this, that: Object) return Boolean is (True);
     
   function getLmcpTypeName(this : Object) return String is ("Object");
   
   function getFullLmcpTypeName(this : Object) return String is ("avtas.lmcp.object.Object");
   
   function getLmcpType(this : Object) return UInt32_t is (0);
   
   function getSeriesName(this : Object) return String is ("");
   
   function getSeriesNameAsLong(this : Object) return Int64_t is (0);
   
   function getSeriesVersion(this : Object) return UInt16_t is (0);

   function calculatePackedSize(this : Object) return UInt32_t is (0);
   
end avtas.lmcp.object;