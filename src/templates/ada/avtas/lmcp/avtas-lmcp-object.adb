package body AVTAS.LMCP.Object is

   procedure XML_Output (this  : Object'Class;
                         S     : access Ada.Streams.Root_Stream_Type'Class;
                         Level : Natural := 0) is
   begin
      String'Write (S, LeftPad ("<" & this.getLmcpTypeName & " Series=""" & this.getSeriesName & """>" & ASCII.LF, Level));
      this.XML_Write (S, Level + 1);  -- dynamically dispatch
      String'Write (S, LeftPad ("</" & this.getLmcpTypeName & ">" & ASCII.LF, Level));
   end XML_Output;

end AVTAS.LMCP.Object;

