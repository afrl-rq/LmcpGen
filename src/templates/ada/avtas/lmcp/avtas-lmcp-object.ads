with AVTAS.LMCP.Types;       use AVTAS.LMCP.Types;
with AVTAS.LMCP.ByteBuffers; use AVTAS.LMCP.ByteBuffers;
with Ada.Streams;
with Utilities;

package AVTAS.LMCP.Object is

   type Object is abstract tagged null record;
   type Object_Acc is access all Object;
   type Object_Any is access all Object'Class;

--     function clone(this, that: access Object) return Object_Acc is abstract;

   overriding
   function "=" (This, That : Object) return Boolean is (True);  -- FIXME

   function getLmcpTypeName(this : Object) return String is ("Object");

   function getFullLmcpTypeName(this : Object) return String is ("avtas.lmcp.object.Object");

   function getLmcpType(this : Object) return UInt32 is (0);

   function getSeriesName(this : Object) return String is ("");

   function getSeriesNameAsLong(this : Object) return Int64 is (0);

   function getSeriesVersion(this : Object) return UInt16 is (0);

   function calculatePackedSize(this : Object) return UInt32 is (0);

   procedure Pack (This : Object; Buf : in out ByteBuffer) is null;

   procedure Unpack (This : out Object; Buf : in out ByteBuffer) is null;

   -- XML output
   function LeftPad is new Utilities.LeftPad (Width => 2);

   procedure XML_Output (this  : Object'Class;
                         S     : access Ada.Streams.Root_Stream_Type'Class;
                         Level : Natural := 0);

   procedure XML_Write (this  : Object;
                        S     : access Ada.Streams.Root_Stream_Type'Class;
                        Level : Natural) is null;
   
end AVTAS.LMCP.Object;

