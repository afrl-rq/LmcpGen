with AVTAS.LMCP.Types;       use AVTAS.LMCP.Types;
with AVTAS.LMCP.ByteBuffers; use AVTAS.LMCP.ByteBuffers;
with AVTAS.LMCP.Object;      use AVTAS.LMCP.Object;

package AVTAS.LMCP.Factory is

   HEADER_SIZE      : constant UInt32 := 8;
   CHECKSUM_SIZE    : constant UInt32 := 4;
   SERIESNAME_SIZE  : constant UInt32 := 8;
   LMCP_CONTROL_STR : constant Int32  := 16#4c4d4350#;
   
   function packMessage(rootObject : in avtas.lmcp.object.Object_Any; enableChecksum : in Boolean) return ByteBuffer;
   procedure putObject(object : in avtas.lmcp.object.Object_Any; buffer : in out ByteBuffer);
   procedure getObject(buffer : in out ByteBuffer; output : out avtas.lmcp.object.Object_Any);
   function createObject(seriesId : in Int64;  msgType : in UInt32; version: in UInt16) return avtas.lmcp.object.Object_Any;

   function CalculateChecksum (Buffer : in ByteBuffer; Last : in Index) return UInt32;
   --  Computes the modular checksum for the Buffer contents from 1 .. Last. Assumes
   --  Big Endian byte order.

   function getObjectSize(buffer : in ByteBuffer) return UInt32;

   function Validate (Buffer : in ByteBuffer) return Boolean;
   --  Validates a buffer by comparing a newly computed checksum with the
   --  previously computed checksum value stored with the message
   --  in the buffer. Assumes the buffer is in Big Endian byte order.

end AVTAS.LMCP.Factory;
