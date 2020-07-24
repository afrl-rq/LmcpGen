with -<full_parent_datatype_package>-; use -<full_parent_datatype_package>-;
with -<full_series_name_dots>-.enumerations; use -<full_series_name_dots>-.enumerations;
with avtas.lmcp.byteBuffers; use avtas.lmcp.byteBuffers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;
with Ada.Containers.Indefinite_Vectors;

-<with_all_field_types>-

package -<full_series_name_dots>-.-<datatype_name>- is

   type -<datatype_name>- is new -<full_parent_datatype>- with private;
   type -<datatype_name>-_Acc is access all -<datatype_name>-;
   type -<datatype_name>-_Any is access all -<datatype_name>-'Class;

   Subscription : constant String := "-<full_datatype_name_dots>-";
   TypeName     : constant String := "-<datatype_name>-";
   SeriesName   : constant String := "-<series_name>-";
   TypeId       : constant := -<series_name>-Enum'Pos(-<datatype_name_caps>-_ENUM) + 1;
   --  The other languages start their enums at 1, whereas Ada starts at 0,
   --  so add 1. This is critical in the case statement (switch) in the 
   --  factories so that an object of the right subclass is created.

   -<vector_package_import>-

   overriding
   function getFullLmcpTypeName(this : -<datatype_name>-) return String is (Subscription);

   overriding
   function getLmcpTypeName(this : -<datatype_name>-) return String is (TypeName);

   overriding
   function getLmcpType(this : -<datatype_name>-) return UInt32 is (TypeId);

   -<get_and_set_methods_spec>-

   overriding
   function calculatePackedSize(this: -<datatype_name>-) return UInt32;

   overriding
   procedure Pack (This : -<datatype_name>-; Buffer : in out ByteBuffer);
   
   overriding
   procedure Unpack (This : out -<datatype_name>-; Buffer : in out ByteBuffer);

   --function toString(this : -<datatype_name>-; depth : Integer) return String;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors 
      (Index_Type => Positive, Element_Type => String);
   --  TODO: move to a single, shared instantiation

   function -<datatype_name>-_Descendants return String_Vectors.Vector;

private

   type -<datatype_name>- is new -<full_parent_datatype>- with record
      -<record_fields>-
   end record;

   overriding
   procedure XML_Write (this  : -<datatype_name>-;
                        S     : access Ada.Streams.Root_Stream_Type'Class;
                        Level : Natural);

end -<full_series_name_dots>-.-<datatype_name>-;

