with avtas.lmcp.types; use avtas.lmcp.types;
with avtas.lmcp.byteBuffers; use avtas.lmcp.byteBuffers;

package avtas.lmcp.object is
   
   type Object is tagged null record;
   type Object_Acc is access all Object;
   type Object_Any is access all Object'Class;
   
   function clone(this, that: Object_Acc) return Object_Acc is abstract;
   
   function "="(this, that: Object) return Boolean is (True);
     
   function getLmcpTypeName(this : Object) return String is ("Object");
   
   function getFullLmcpTypeName(this : Object) return String is ("avtas.lmcp.object.Object");
   
   function getLmcpType(this : Object) return UInt32 is (0);
   
   function getSeriesName(this : Object) return String is ("");
   
   function getSeriesNameAsLong(this : Object) return Int64 is (0);
   
   function getSeriesVersion(this : Object) return UInt16 is (0);

   function calculatePackedSize(this : Object) return UInt32 is (0);

   procedure pack(object_acc : in Object_Any; buf : in out ByteBuffer);
   procedure unpack(buf : in out ByteBuffer; object_acc : in out Object_Any);
   
end avtas.lmcp.object;
