with avtas.lmcp.factory; use avtas.lmcp.factory;
with Ada.Characters.Latin_1;

package body -<full_series_name_dots>-.-<datatype_name>- is

   -<get_and_set_methods_body>-
   -<calculate_packed_size_body>-

   -<pack_body>-

   -<unpack_body>-

   -<xml_write_body>-

   function -<datatype_name>-_Descendants return String_Vectors.Vector is      
      Descendants : String_Vectors.Vector; 
      use String_Vectors;
   begin
      -<all_descendants>-
      return Descendants;
   end -<datatype_name>-_Descendants;

end -<full_series_name_dots>-.-<datatype_name>-;

