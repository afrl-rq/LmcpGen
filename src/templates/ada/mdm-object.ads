with avtas.lmcp.object; use avtas.lmcp.object;
with avtas.lmcp.types; use avtas.lmcp.types;
with avtas.lmcp.byteBuffers; use avtas.lmcp.byteBuffers;
with -<full_series_name_dots>-.enumerations; use -<full_series_name_dots>-.enumerations;

package -<full_series_name_dots>-.object is
   
   type Object is abstract new avtas.lmcp.object.Object with private;
   type Object_Acc is access all Object;
   type Object_Any is access all Object'Class;
   
   function getSeriesVersion(this : Object) return UInt16_t is (-<series_version>-);
   function getSeriesName(this : Object) return String is ("-<series_name>-");
   function getSeriesNameAsLong(this : Object) return Int64_t is (-<series_id>-);

   procedure pack(this: in Object_Any; buf: in out ByteBuffer);
   procedure unpack(this: in out Object_Any; buf: in out ByteBuffer);
   
private
   
   type Object is new avtas.lmcp.object.Object with null record;

end -<full_series_name_dots>-.object;
