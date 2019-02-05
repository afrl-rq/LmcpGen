with avtas.lmcp.types; use avtas.lmcp.types;
with avtas.lmcp.byteBuffers; use avtas.lmcp.byteBuffers;
with avtas.lmcp.object; use avtas.lmcp.object;
with avtas.lmcp.types; use avtas.lmcp.types;

package avtas.lmcp.factory is

   HEADER_SIZE : constant UInt32 := 8;
   CHECKSUM_SIZE : constant UInt32 := 4;
   SERIESNAME_SIZE : constant UInt32 := 8;
   LMCP_CONTROL_STR : constant Int32 := 16#4c4d4350#;
   
   function packMessage(rootObject : in avtas.lmcp.object.Object_Any; enableChecksum : in Boolean) return ByteBuffer;
   procedure putObject(object : in avtas.lmcp.object.Object_Any; buffer : in out ByteBuffer);
   procedure getObject(buffer : in out ByteBuffer; output : out avtas.lmcp.object.Object_Any);
   function createObject(seriesId : in Int64;  msgType : in UInt32; version: in UInt16) return avtas.lmcp.object.Object_Any;
   function calculateChecksum(buffer : in ByteBuffer) return UInt32;
   function getObjectSize(buffer : in ByteBuffer) return UInt32;
   function validate(buffer : in ByteBuffer) return Boolean;

end avtas.lmcp.factory;
