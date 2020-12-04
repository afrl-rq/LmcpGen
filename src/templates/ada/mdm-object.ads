with AVTAS.LMCP.ByteBuffers; use AVTAS.LMCP.ByteBuffers;

package -<full_series_name_dots>-.Object is

   type Object is abstract new AVTAS.LMCP.Object.Object with private;
   type Object_Acc is access all Object;
   type Object_Any is access all Object'Class;

   overriding function getSeriesVersion(this : Object) return UInt16 is (-<series_version>-);
   overriding function getSeriesName(this : Object) return String is ("-<series_name>-");
   overriding function getSeriesNameAsLong(this : Object) return Int64 is (-<series_id>-);

 private

   type Object is abstract new AVTAS.LMCP.Object.Object with null record;

end -<full_series_name_dots>-.Object;

