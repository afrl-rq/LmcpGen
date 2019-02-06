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

   Subscription : constant String := "-<full_datatype_name_dots>-";
   TypeName     : constant String := "-<datatype_name>-";
   SeriesName   : constant := -<series_name>-Enum'Pos(-<datatype_name_caps>-_ENUM);

   -<vector_package_import>-

   function dummy return Integer;

   overriding
   function getFullLmcpTypeName(this : -<datatype_name>-) return String is (Subscription);

   overriding
   function getLmcpTypeName(this : -<datatype_name>-) return String is (TypeName);

   overriding
   function getLmcpType(this : -<datatype_name>-) return UInt32 is (SeriesName);

   -<get_and_set_methods_spec>-

   overriding
   function calculatePackedSize(this: -<datatype_name>-) return UInt32;

   procedure pack(object_acc : in -<datatype_name>-_-<access_suffix>-; buf : in out ByteBuffer);
   procedure unpack(object_acc : in out -<datatype_name>-_-<access_suffix>-; buf : in out ByteBuffer);

   --function toString(this : -<datatype_name>-; depth : Integer) return String;

private

   type -<datatype_name>- is new -<full_parent_datatype>- with record
      -<record_fields>-
   end record;

end -<full_series_name_dots>-.-<datatype_name>-;
