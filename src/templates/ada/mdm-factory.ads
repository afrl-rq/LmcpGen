with avtas.lmcp.types; use avtas.lmcp.types;
with avtas.lmcp.byteBuffers; use avtas.lmcp.byteBuffers;
with avtas.lmcp.object; use avtas.lmcp.object;
with avtas.lmcp.types; use avtas.lmcp.types;

package -<full_series_name_dots>-.factory is

   HEADER_SIZE : constant := 8;
   CHECKSUM_SIZE : constant := 4;
   SERIESNAME_SIZE : constant := 8;
   LMCP_CONTROL_STR : constant := 16#4c4d4350#;

   function packMessage(rootObject : in avtas.lmcp.object.Object_Any; enableChecksum : in Boolean) return ByteBuffer;
   procedure getObject(buffer : in out ByteBuffer; output : out avtas.lmcp.object.Object_Any);
   function createObject(seriesId : in Int64;  msgType : in UInt32; version: in UInt16) return avtas.lmcp.object.Object_Any;
   function calculateChecksum(buffer : in ByteBuffer) return UInt32;
   function getObjectSize(buffer : in ByteBuffer) return UInt32;
   function validate(buffer : in ByteBuffer) return Boolean;

end -<full_series_name_dots>-.factory;
