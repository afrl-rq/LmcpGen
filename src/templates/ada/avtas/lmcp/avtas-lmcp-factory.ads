with AVTAS.LMCP.Types;       use AVTAS.LMCP.Types;
with AVTAS.LMCP.ByteBuffers; use AVTAS.LMCP.ByteBuffers;
with AVTAS.LMCP.Object;      use AVTAS.LMCP.Object;

package AVTAS.LMCP.Factory is

   HEADER_SIZE      : constant := 8;
   CHECKSUM_SIZE    : constant := 4;
   SERIESNAME_SIZE  : constant := 8;
   LMCP_CONTROL_STR : constant := 16#4c4d4350#;
   
   function packMessage(rootObject : in avtas.lmcp.object.Object_Any; enableChecksum : in Boolean) return ByteBuffer;
   procedure putObject(object : in avtas.lmcp.object.Object_Any; buffer : in out ByteBuffer);
   procedure getObject(buffer : in out ByteBuffer; output : out avtas.lmcp.object.Object_Any);
   function createObject(seriesId : in Int64;  msgType : in UInt32; version: in UInt16) return avtas.lmcp.object.Object_Any;

   function CalculatedChecksum (Buffer : in ByteBuffer; Size : Index) return UInt32;
   --  Computes the modular checksum for the Buffer contents, ignoring the
   --  last 4 bytes in which the checksum may or may not be stored. Assumes
   --  Big Endian byte order.

   function getObjectSize(buffer : in ByteBuffer) return UInt32;

   function Validated (Buffer : in ByteBuffer) return Boolean;
   --  Returns whether a newly computed checksum equals the previously computed
   --  checksum value stored with the message in the buffer. Assumes the buffer
   --  is in Big Endian byte order.

end AVTAS.LMCP.Factory;
