with avtas.lmcp.types; use avtas.lmcp.types;
with afrl.cmasi.object; use afrl.cmasi.object;
with afrl.cmasi.enumerations; use afrl.cmasi.enumerations;
with Ada.Containers.Vectors;

package afrl.cmasi.operatingRegion is

   type OperatingRegion is new afrl.cmasi.object.Object with private;
   type OperatingRegion_Acc is access all OperatingRegion;
   
   package Vect_Int64_t is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Int64_t);
   type Vect_Int64_t_Acc is access all Vect_Int64_t.Vector;
   
   function getFullLmcpTypeName(this : OperatingRegion) return String;
   function getLmcpTypeName(this : OperatingRegion) return String;
   function getLmcpType(this : OperatingRegion) return UInt32_t;
   function getID(this : OperatingRegion'Class) return Int64_t;
   procedure setID(this : out OperatingRegion'Class; ID : in Int64_t);
   function getKeepInAreas(this : OperatingRegion'Class) return Vect_Int64_t_Acc;
   function getKeepOutAreas(this : OperatingRegion'Class) return Vect_Int64_t_Acc;
   
private
   
   type OperatingRegion is new afrl.cmasi.object.Object with record
      ID : Int64_t := 0;
      KeepInAreas : Vect_Int64_t_Acc := new Vect_Int64_t.Vector;
      KeepOutAreas : Vect_Int64_t_Acc := new Vect_Int64_t.Vector;
   end record;
   
end afrl.cmasi.operatingRegion;
