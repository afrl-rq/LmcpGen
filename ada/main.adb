with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with avtas.lmcp; use avtas.lmcp;
with afrl.cmasi; use afrl.cmasi;
with afrl.cmasi_enumerations; use afrl.cmasi_enumerations;
with Interfaces; use Interfaces;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is

   kz : KeepInZone;
   c : Circle;
   l3d : Location3D;
   l3d_Acc : Location3D_Acc;
   az : AbstractZone;
   vect : VectInt64.Vector;
   ubs : Unbounded_String;

begin

   l3d.setLatitude(30.43258611);
   l3d.setLongitude(-87.17408333);
   l3d.setAltitude(600.0);
   l3d.setAltitudeType(afrl.cmasi_enumerations.MSL);

   l3d_Acc := new Location3D;
   l3d_Acc.setLatitude(30.43258611);
   l3d_Acc.setLongitude(-87.17408333);
   l3d_Acc.setAltitude(600.0);

   c.setRadius(100.0);
   c.setCenterPoint(l3d_Acc);
   ubs := c.toString(3);
   Put(To_String(ubs));
   New_Line;


   az.getAffectedAircraft.Append(10);
   az.getAffectedAircraft.Append(11);
   az.getAffectedAircraft.Append(12);
   Put(az.getAffectedAircraft.First_Element'Image);
   for i of az.getAffectedAircraft.all loop
      Put(i'Image);
   end loop;
   az.setLabel(Unbounded_String'(To_Unbounded_String("AzLabel")));
   New_Line;
   Put(To_String(az.getLabel));

  -- kz.setZoneId(10);
  -- kz.setMinAltitude(100.0);
  -- kz.setMaxAltitude(750.0);
--   kz.setBoundary(c);


end Main;
