with avtas.lmcp.types; use avtas.lmcp.types;
with afrl.cmasi.object; use afrl.cmasi.object;
with afrl.cmasi.enumerations; use afrl.cmasi.enumerations;
with afrl.cmasi.AbstractGeometry; use afrl.cmasi.AbstractGeometry;
with afrl.cmasi.location3d; use afrl.cmasi.location3d;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package afrl.cmasi.circle is
   
   type Circle is new AbstractGeometry.AbstractGeometry with private;
   type Circle_Acc is access all Circle;

   function getFullLmcpTypeName(this : Circle) return String;
   function getLmcpTypeName(this : Circle) return String;
   function getLmcpType(this : Circle) return UInt32_t;
   function getRadius(this : Circle'Class) return Float_t;
   procedure setRadius(this : out Circle'Class; Radius : in Float_t);
   function getCenterPoint(this : Circle'Class) return Location3D_Acc;
   procedure setCenterPoint(this : out Circle'Class; CenterPoint : in Location3D_Acc);
   function toString(this : Circle'Class; depth : Integer) return String;
   
private
   
   type Circle is new AbstractGeometry.AbstractGeometry with record 
      Radius : Float_t := 0.0;
      CenterPoint : Location3D_Acc;
   end record;

end afrl.cmasi.circle;
