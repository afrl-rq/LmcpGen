package body Utilities is

   function LeftPad (Str: String; Level : Natural) return String is
      Padding: constant String (1 .. Width * Level) := (others => ' ');
   begin
      return Padding & Str;
   end LeftPad;

end Utilities;
