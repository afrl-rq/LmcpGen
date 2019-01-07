with -<full_parent_datatype>-; use -<full_parent_datatype>-;
with -<full_series_name_dots>-.enumerations; use -<full_series_name_dots>-.enumerations;
with avtas.lmcp.types; use avtas.lmcp.types;

-<with_all_field_types>-

package -<full_series_name_dots>-.-<datatype_name>- is

   type -<datatype_name>- is new -<full_parent_datatype>- with private;
   type -<datatype_name>-_Acc is access all -<datatype_name>-;
   type -<datatype_name>-_Any is access all -<datatype_name>-'Class;
   
   -<vector_package_import>-

   function getFullLmcpTypeName(this : -<datatype_name>-) return String ("-<full_datatype_name_dots>-");
   function getLmcpTypeName(this : -<datatype_name>-) return String ("-<datatype_name>-");
   function getLmcpType(this : -<datatype_name>-) return UInt32_t (-<series_name>-Enum'Pos(-<datatype_name_caps>-_ENUM));

   -<get_and_set_methods_spec>-

private
   
   type -<datatype_name>- is new -<full_parent_datatype>- with record
      -<record_fields>-
   end record;

end -<full_series_name_dots>-.-<datatype_name>-;
