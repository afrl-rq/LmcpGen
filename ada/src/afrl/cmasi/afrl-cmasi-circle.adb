with Ada.Characters.Latin_1;

package body afrl.cmasi.circle is

   function getFullLmcpTypeName(this : Circle) return String is ("afrl.cmasi.circle.Circle");

   function getLmcpTypeName(this : Circle) return String is ("Circle");

   function getLmcpType(this : Circle) return UInt32_t is (CMASIEnum'Pos(CIRCLE_ENUM));

   function getRadius(this : Circle'Class) return Float_t is (this.Radius);

   procedure setRadius(this : out Circle'Class; Radius : in Float_t) is
   begin
      this.Radius := Radius;
   end setRadius;

   function getCenterPoint (this : Circle'Class) return Location3D_Acc is (this.CenterPoint);

   procedure setCenterPoint(this : out Circle'Class; CenterPoint : in Location3D_Acc) is
   begin
      this.CenterPoint := CenterPoint;
   end setCenterPoint;

   function toString(this : Circle'Class; depth : Integer) return String is
   begin
      declare
         depth_copy : Integer := depth;
         UBS : Unbounded_String;
         indent : Unbounded_String;
         LF : Unbounded_String := To_Unbounded_String(String'(1 => Ada.Characters.Latin_1.LF));
      begin
         indent := To_Unbounded_String(String'(1 .. depth_copy*3 => ' '));
         UBS := UBS & indent & "Object ( Circle ) {" & LF;
         depth_copy := depth_copy + 1;
         indent := To_Unbounded_String(String'(1 .. depth_copy*3 => ' '));
         UBS := UBS & indent & "CenterPoint (Location3D)";
         if(this.CenterPoint = null) then
            UBS := UBS & " = null";
         end if;
         UBS := UBS & LF;
         UBS := UBS & indent & "Radius (float) = " & To_Unbounded_String(this.Radius'Image) & LF;
         depth_copy := depth_copy - 1;
         return To_String(UBS);
      end;
   end toString;

end afrl.cmasi.circle;
