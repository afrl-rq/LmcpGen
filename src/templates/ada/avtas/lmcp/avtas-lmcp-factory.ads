with avtas.lmcp.types;       use avtas.lmcp.types;
with avtas.lmcp.byteBuffers; use avtas.lmcp.byteBuffers;
with avtas.lmcp.object;      use avtas.lmcp.object;

package avtas.lmcp.factory is

   HEADER_SIZE      : constant UInt32 := 8;
   CHECKSUM_SIZE    : constant UInt32 := 4;
   SERIESNAME_SIZE  : constant UInt32 := 8;
   LMCP_CONTROL_STR : constant Int32  := 16#4c4d4350#;
   
   function packMessage(rootObject : in avtas.lmcp.object.Object_Any; enableChecksum : in Boolean) return ByteBuffer;
   procedure putObject(object : in avtas.lmcp.object.Object_Any; buffer : in out ByteBuffer);
   procedure getObject(buffer : in out ByteBuffer; output : out avtas.lmcp.object.Object_Any);
   function createObject(seriesId : in Int64;  msgType : in UInt32; version: in UInt16) return avtas.lmcp.object.Object_Any;

   function CalculateChecksum (Buffer : in ByteBuffer; Last : UInt32) return UInt32 with
     Pre => Last in 1 .. Buffer.Capacity;
   --  Computes the modular checksum for the Buffer contents. Assumes
   --  Big Endian order.
   --
   --  Last is the index of the last byte used within Buffer's internal byte
   --  array to hold a complete message, including the checksum (regardless
   --  of whether the checksum has already been stored there).
   --
   --  The checksum calculation does not include those bytes that either will,
   --  or already do hold the UInt32 checksum stored with the message in the
   --  buffer. Those checksum bytes are at the very end so this routine will
   --  subtract a UInt32's number of bytes from Last in order to skip those
   --  bytes. The caller is responsible for passing a value to Last such that
   --  this subtraction yields the index of the last byte of the message prior
   --  to the checksum's bytes.
   
   function getObjectSize(buffer : in ByteBuffer) return UInt32;

   function Validate (Buffer : in ByteBuffer) return Boolean;
   --  Validates a buffer by comparing a newly computed checksum with the
   --  previously computed checksum value stored with the message
   --  in the buffer. Assumes the buffer is in Big Endian byte order.
   --
   --  Assumes the message is complete, including the checksum, such that
   --  Buffer.Position is at the end.

end avtas.lmcp.factory;
