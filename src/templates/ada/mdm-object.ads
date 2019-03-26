with avtas.lmcp.byteBuffers; use avtas.lmcp.byteBuffers;

package -<full_series_name_dots>-.object is

   type Object is abstract new avtas.lmcp.object.Object with private;
   type Object_Acc is access all Object;
   type Object_Any is access all Object'Class;

   overriding function getSeriesVersion(this : Object) return UInt16 is (-<series_version>-);
   overriding function getSeriesName(this : Object) return String is ("-<series_name>-");
   overriding function getSeriesNameAsLong(this : Object) return Int64 is (-<series_id>-);

 private

   type Object is abstract new avtas.lmcp.object.Object with null record;

end -<full_series_name_dots>-.object;

