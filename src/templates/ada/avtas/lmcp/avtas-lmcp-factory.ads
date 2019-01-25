with avtas.lmcp.types; use avtas.lmcp.types;
with avtas.lmcp.byteBuffers; use avtas.lmcp.byteBuffers;
with avtas.lmcp.object; use avtas.lmcp.object;
with avtas.lmcp.types; use avtas.lmcp.types;

package avtas.lmcp.factory is

   HEADER_SIZE : constant UInt32_t := 8;
   CHECKSUM_SIZE : constant UInt32_t := 4;
   SERIESNAME_SIZE : constant UInt32_t := 8;
   LMCP_CONTROL_STR : constant Int32_t := 16#4c4d4350#;
   
   function packMessage(rootObject : in avtas.lmcp.object.Object_Any; enableChecksum : in Boolean) return ByteBuffer;
   procedure putObject(object : in avtas.lmcp.object.Object_Any; buffer : in out ByteBuffer);
   procedure getObject(buffer : in out ByteBuffer; output : out avtas.lmcp.object.Object_Any);
   function createObject(seriesId : in Int64_t;  msgType : in UInt32_t; version: in UInt16_t) return avtas.lmcp.object.Object_Any;
   function calculateChecksum(buffer : in ByteBuffer) return UInt32_t;
   function getObjectSize(buffer : in ByteBuffer) return UInt32_t;
   function validate(buffer : in ByteBuffer) return Boolean;

end avtas.lmcp.factory;
