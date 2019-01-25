with -<full_parent_datatype_package>-; use -<full_parent_datatype_package>-;
with -<full_series_name_dots>-.enumerations; use -<full_series_name_dots>-.enumerations;
with avtas.lmcp.types; use avtas.lmcp.types;
with avtas.lmcp.byteBuffers; use avtas.lmcp.byteBuffers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-<with_all_field_types>-

package -<full_series_name_dots>-.-<datatype_name>- is

   type -<datatype_name>- is new -<full_parent_datatype>- with private;
   type -<datatype_name>-_Acc is access all -<datatype_name>-;
   type -<datatype_name>-_Any is access all -<datatype_name>-'Class;
   
   -<vector_package_import>-

   function dummy return Integer;

   function getFullLmcpTypeName(this : -<datatype_name>-) return String is ("-<full_datatype_name_dots>-");
   function getLmcpTypeName(this : -<datatype_name>-) return String is ("-<datatype_name>-");
   function getLmcpType(this : -<datatype_name>-) return UInt32_t is (-<series_name>-Enum'Pos(-<datatype_name_caps>-_ENUM)+1);

   -<get_and_set_methods_spec>-
   function calculatePackedSize(this: -<datatype_name>-) return UInt32_t;

   procedure pack(this: in -<datatype_name>-_Acc; buf: in out ByteBuffer);
   procedure unpack(this: in out -<datatype_name>-_Acc; buf: in out ByteBuffer);

   --function toString(this : -<datatype_name>-; depth : Integer) return String;

private
   
   type -<datatype_name>- is new -<full_parent_datatype>- with record
      -<record_fields>-
   end record;

end -<full_series_name_dots>-.-<datatype_name>-;
