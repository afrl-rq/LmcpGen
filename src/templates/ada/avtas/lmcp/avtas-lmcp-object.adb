package body avtas.lmcp.object is

   procedure pack(object_acc : in Object_Any; buf : in out ByteBuffer) is
   begin
      null;
   end pack;

   procedure unpack(buf : in out ByteBuffer; object_acc : in out Object_Any) is
   begin
      null;
   end unpack;

   procedure XML_Output (this  : Object'Class;
                         S     : access Ada.Streams.Root_Stream_Type'Class;
                         Level : Natural := 0) is
   begin
      String'Write (S, LeftPad ("<" & this.getLmcpTypeName & " Series=""" & this.getSeriesName & """>" & ASCII.LF, Level));
      this.XML_Write (S, Level + 1);
      String'Write (S, LeftPad ("</" & this.getLmcpTypeName & ">" & ASCII.LF, Level));
   end XML_Output;

end avtas.lmcp.object;
