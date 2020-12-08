with AVTAS.LMCP.ByteBuffers;  use AVTAS.LMCP.ByteBuffers;

package -<full_series_name_dots>-.Factory is

   HEADER_SIZE      : constant := 8;
   CHECKSUM_SIZE    : constant := 4;
   SERIESNAME_SIZE  : constant := 8;
   LMCP_CONTROL_STR : constant := 16#4c4d4350#;

   function packMessage(rootObject : in avtas.lmcp.object.Object_Any; enableChecksum : in Boolean) return ByteBuffer;
   procedure getObject(buffer : in out ByteBuffer; output : out avtas.lmcp.object.Object_Any);
   function createObject(seriesId : in Int64;  msgType : in UInt32; version: in UInt16) return avtas.lmcp.object.Object_Any;

   function CalculateChecksum (Buffer : in ByteBuffer) return UInt32;
   --  Computes the modular checksum for the Buffer contents. Assumes
   --  Big Endian order.
   --
   --  The checksum calculation does not include those bytes that either will,
   --  or already do hold the UInt32 checksum stored at the very end of the
   --  buffer

   function getObjectSize(buffer : in ByteBuffer) return UInt32;

   function Validate (Buffer : in ByteBuffer) return Boolean;
   --  Validates a buffer by comparing a newly computed checksum with the
   --  previously computed checksum value stored with the message
   --  in the buffer. Assumes the buffer is in Big Endian byte order.

end -<full_series_name_dots>-.Factory;

